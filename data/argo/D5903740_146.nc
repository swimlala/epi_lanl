CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-30T09:16:00Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160330091600  20160531170830  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_146                   2C  D   APEX                            5374                            041511                          846 @נ��e�1   @נ�1�b�@:X�t�j�c���E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy` D�  D�C3D�� D��fD��D�P D���D��fD�fD�C3D��fD���D��D�@ Dڃ3D��3D�	�D�FfD�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy\D�/�D�R�D���D��D�{D�_�D��{D��D�D�R�D��D��{D�{D�O�Dڒ�D���D�HD�VD�vD��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��mA��A��A��A��mA��
A���A���A���A���A���A�ȴA���A��wA���A�r�A�bNA�M�A�9XA�oA��yA���A�v�A�ffA�XA�E�A�"�A��A�VA�A��A��A��HA���A���A���A�n�A�\)A�XA�XA�M�A�K�A�O�A�M�A�K�A�E�A�?}A�5?A�5?A�-A�JA��jA��A���A���A���A�r�A�=qA���A��\A�VA��!A�"�A�l�A�p�A�dZA�JA�5?A��RA��DA���A�S�A�`BA��jA�bNA���A��RA��;A�XA��hA���A�r�A���A��;A�VA��A�ȴA��A�
=A��DA��HA��A�r�A���A�`BA��A�7LA��A���A�33A��^A�t�A��A�Q�A�M�A�JA��HA� �A��`A�(�A�A���A�A�^5A�A�~�A� �A��TA�z�A�?}A�A|�Az��Ax$�At-ApĜAn-Alz�Aj��Ai�PAg
=Ac`BAb  A`r�A`JA^��A]�TA]��A[dZAZ��AZ{AX{AW�FAWdZAV^5ASS�AP�AN�/AM|�ALbNAK�#AK"�AH�\AGS�AE`BA@�RA?�A>A�A=p�A<-A:�!A9��A8�A8n�A8E�A7��A7S�A6��A5��A3�FA25?A1�^A1`BA0��A/�A/|�A/;dA-�A+��A*�yA)�A(��A(�yA)A(��A(ĜA'/A%33A#oA"�RA"��A"-A"1A"  A!�;A!�7A!\)A!33A��A%AbNA�^A�hA|�Ap�A�A��A��Av�AbA�AG�A�yA=qA-A��A�A�;A;dA�HA�!AffA�TA5?A
�A	p�A=qA`BA��Ax�A/A7LA�A��A��A ȴ@���@���@��;@���@��^@���@�@�%@�G�@���@�\)@�@�n�@�`B@�j@�;d@��@�1@�-@�1'@�;d@ݑh@�ƨ@�~�@�&�@�Q�@ו�@��@�@��@�ff@�J@���@ղ-@�hs@��`@���@�~�@ёh@�1'@���@�1'@��y@�x�@̓u@���@�M�@�G�@�S�@�-@�&�@Ĵ9@�\)@�o@§�@�$�@�@���@�p�@�%@��@�&�@�C�@�o@���@�$�@�r�@� �@���@���@�;d@��!@�$�@��@�@���@��@�5?@�?}@���@��@�z�@�1@�l�@���@�@��h@�%@��@�Q�@��@���@���@�?}@��;@��@���@��R@��h@��u@�I�@��
@�o@�-@�`B@�%@�Z@�(�@���@�^5@�V@�Ĝ@�/@��`@���@�r�@�1'@� �@��@�dZ@���@�-@��#@��@�G�@�V@���@���@�Ĝ@�j@�  @�;d@�~�@�x�@��@��@�v�@�E�@�M�@�{@���@���@��@�{@��-@�X@��9@��/@�I�@��
@���@�S�@�33@��@���@�M�@��#@�x�@�X@�X@�O�@��/@�bN@��F@�K�@��@���@�v�@�V@��@��@�J@�$�@�/@�bN@�  @��@�S�@�33@�o@�@��@��y@��y@��H@���@��\@�~�@�=q@�@��@���@���@���@��-@��7@�p�@�/@��@���@���@��u@�A�@�(�@��@�1@��;@��w@��@��@�S�@�C�@�"�@�
=@��@���@���@�n�@�-@�J@��@�x�@���@���@��@�bN@�1@;d@~�@~�+@~V@~E�@~$�@~$�@~$�@~$�@~$�@~$�@~{@~@}@}O�@|��@|�/@|��@|(�@{�
@{t�@xĜ@qX@h �@b~�@[�
@S��@L�D@F�R@>�@8bN@2��@+�m@'K�@"J@��@��@~�@�y@	��@E�@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��mA��A��A��A��mA��
A���A���A���A���A���A�ȴA���A��wA���A�r�A�bNA�M�A�9XA�oA��yA���A�v�A�ffA�XA�E�A�"�A��A�VA�A��A��A��HA���A���A���A�n�A�\)A�XA�XA�M�A�K�A�O�A�M�A�K�A�E�A�?}A�5?A�5?A�-A�JA��jA��A���A���A���A�r�A�=qA���A��\A�VA��!A�"�A�l�A�p�A�dZA�JA�5?A��RA��DA���A�S�A�`BA��jA�bNA���A��RA��;A�XA��hA���A�r�A���A��;A�VA��A�ȴA��A�
=A��DA��HA��A�r�A���A�`BA��A�7LA��A���A�33A��^A�t�A��A�Q�A�M�A�JA��HA� �A��`A�(�A�A���A�A�^5A�A�~�A� �A��TA�z�A�?}A�A|�Az��Ax$�At-ApĜAn-Alz�Aj��Ai�PAg
=Ac`BAb  A`r�A`JA^��A]�TA]��A[dZAZ��AZ{AX{AW�FAWdZAV^5ASS�AP�AN�/AM|�ALbNAK�#AK"�AH�\AGS�AE`BA@�RA?�A>A�A=p�A<-A:�!A9��A8�A8n�A8E�A7��A7S�A6��A5��A3�FA25?A1�^A1`BA0��A/�A/|�A/;dA-�A+��A*�yA)�A(��A(�yA)A(��A(ĜA'/A%33A#oA"�RA"��A"-A"1A"  A!�;A!�7A!\)A!33A��A%AbNA�^A�hA|�Ap�A�A��A��Av�AbA�AG�A�yA=qA-A��A�A�;A;dA�HA�!AffA�TA5?A
�A	p�A=qA`BA��Ax�A/A7LA�A��A��A ȴ@���@���@��;@���@��^@���@�@�%@�G�@���@�\)@�@�n�@�`B@�j@�;d@��@�1@�-@�1'@�;d@ݑh@�ƨ@�~�@�&�@�Q�@ו�@��@�@��@�ff@�J@���@ղ-@�hs@��`@���@�~�@ёh@�1'@���@�1'@��y@�x�@̓u@���@�M�@�G�@�S�@�-@�&�@Ĵ9@�\)@�o@§�@�$�@�@���@�p�@�%@��@�&�@�C�@�o@���@�$�@�r�@� �@���@���@�;d@��!@�$�@��@�@���@��@�5?@�?}@���@��@�z�@�1@�l�@���@�@��h@�%@��@�Q�@��@���@���@�?}@��;@��@���@��R@��h@��u@�I�@��
@�o@�-@�`B@�%@�Z@�(�@���@�^5@�V@�Ĝ@�/@��`@���@�r�@�1'@� �@��@�dZ@���@�-@��#@��@�G�@�V@���@���@�Ĝ@�j@�  @�;d@�~�@�x�@��@��@�v�@�E�@�M�@�{@���@���@��@�{@��-@�X@��9@��/@�I�@��
@���@�S�@�33@��@���@�M�@��#@�x�@�X@�X@�O�@��/@�bN@��F@�K�@��@���@�v�@�V@��@��@�J@�$�@�/@�bN@�  @��@�S�@�33@�o@�@��@��y@��y@��H@���@��\@�~�@�=q@�@��@���@���@���@��-@��7@�p�@�/@��@���@���@��u@�A�@�(�@��@�1@��;@��w@��@��@�S�@�C�@�"�@�
=@��@���@���@�n�@�-@�J@��@�x�@���@���@��@�bN@�1@;d@~�@~�+@~V@~E�@~$�@~$�@~$�@~$�@~$�@~$�@~{@~@}@}O�@|��@|�/@|��@|(�@{�
@{t�@xĜ@qX@h �@b~�@[�
@S��@L�D@F�R@>�@8bN@2��@+�m@'K�@"J@��@��@~�@�y@	��@E�@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B(�B,B1'B7LBA�BC�BC�BD�BD�BD�BE�BF�BG�BG�BG�BG�BG�BG�BD�BB�BC�BC�BC�BE�BH�BI�BI�BI�BI�BI�BJ�BI�BI�BF�B@�B<jB=qB=qB?}B@�B@�B?}B9XB=qB=qB33B�B�B��B�sB�/B��B�}B�B�VB�B�PB�\B�=B�Bu�BiyBZBJ�BG�BE�BA�B-B�BÖB�qB�LB�!B��B��B�JB�By�BaHB\)BXBS�BL�BE�B@�B7LB(�B�BB
�fB
ǮB
�9B
��B
�oB
�hB
�hB
�hB
�B
�B
|�B
w�B
s�B
p�B
iyB
N�B
6FB
�B	��B	�B	�wB	�'B	��B	��B	��B	�1B	�B	x�B	t�B	l�B	dZB	`BB	T�B	P�B	K�B	D�B	B�B	?}B	6FB	#�B	�B	hB	�B	oB	\B		7B��B�B�;B�jB�B�B�'B�B�B��B��B��B��B��B�oB�oB�oB�bB�oB�uB��B��B��B��B��B��B��B��B�DB�DB�PB�\B�VB�DB�1B�%B�B�B�B�B�B� B� B~�B|�B{�Bw�Bu�Bs�Bq�Bq�Bp�Bo�Bl�BhsBffBdZBaHB\)BZBXBS�BM�BI�BE�BB�BA�B?}B>wB=qB9XB6FB2-B/B-B+B)�B(�B)�B,B-B(�B �B�B�B�B�B�B�B{B{B\BVBVBPBPBJBJBPB\BhBhBhBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B%�B&�B(�B(�B(�B(�B'�B'�B'�B+B.B33B5?B=qB@�BB�BB�BC�BD�BE�BE�BE�BF�BF�BE�BF�BG�BH�BJ�BJ�BM�BR�BS�BR�BR�BYBYBZBZB[#B\)B]/B`BBbNBdZBffBgmBhsBgmBgmBffBhsBjBp�Bp�Br�Bs�Bu�Bz�B|�B� B�B�1B�DB�JB�VB�VB�bB�uB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�3B�9B�?B�?B�?B�FB�RB�XB�qBBÖB��BĜBȴB��B��B��B��B�
B�B�#B�/B�5B�HB�fB�B�B�B�B�B�B�B��B��B��B	B	B	B	B	B	%B	1B		7B	
=B	DB	bB	uB	�B	�B	�B	�B	"�B	&�B	(�B	+B	,B	.B	0!B	0!B	0!B	0!B	2-B	5?B	6FB	:^B	<jB	=qB	>wB	?}B	@�B	A�B	C�B	E�B	G�B	H�B	I�B	K�B	N�B	R�B	S�B	T�B	VB	W
B	YB	ZB	[#B	]/B	^5B	_;B	_;B	aHB	cTB	cTB	e`B	gmB	hsB	iyB	m�B	q�B	s�B	t�B	v�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�JB	�PB	�VB	�bB	�hB	�{B	��B	�qB	�/B	�B	��B
DB
�B
�B
)�B
33B
9XB
B�B
H�B
N�B
VB
[#B
bNB
gmB
m�B
r�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BbBhBbBbBkBfBfBhBfBhBhBfBkBhBkBfBlBlByB!�B$�B(�B+�B0�B7!BA^BClBCpBDvBDtBDsBExBF|BG�BG�BG�BG�BG�BG�BDrBBgBCqBCrBCpBEyBH�BI�BI�BI�BI�BI�BJ�BI�BI�BF�B@[B<BB=GB=GB?UB@VB@ZB?SB91B=JB=HB3B�B�~B��B�IB�B��B�VB��B�'B��B�#B�1B�B��Bu�BiJBY�BJ�BG�BEqBA]B,�B�hB�cB�CB�B��B��B�RB�B��By�BaB[�BW�BS�BL�BErB@VB7B(�BZB �B
�:B
�B
�B
��B
�@B
�;B
�=B
�=B
��B
��B
|�B
w�B
s�B
pzB
iLB
N�B
6B
�B	��B	��B	�NB	��B	��B	��B	��B	�	B	��B	x�B	t�B	ldB	d3B	`B	T�B	P�B	K�B	DuB	BiB	?WB	6 B	#�B	[B	CB	\B	JB	8B		B��B��B�B�HB��B��B�B��B��B��B��B��B�~B�mB�NB�NB�OB�CB�MB�SB�fB��B��B��B��B��B�kB�_B�%B�!B�.B�:B�2B�"B�B�B��B��B��B��B��B�B�B~�B|�B{�Bw�Bu�Bs�Bq�Bq�Bp�Bo}BlgBhQBfEBd8Ba(B\BY�BW�BS�BM�BI�BE�BBnBAhB?\B>WB=5B98B6#B2B.�B,�B*�B)�B(�B)�B+�B,�B(�B �B|B�B{BzBDBDBAB=B<BBBBBBBBB+B,B*B2BRBBBHBkBTBvB}B|B~B]B\B`BBZBuBqB{BwBxB�B �B%�B&�B(�B(�B(�B(�B'�B'�B'�B*�B-�B3B5B=KB@ZBBiBBiBCpBDuBE~BE|BEyBF�BF�BEzBF�BG�BH�BJ�BJ�BM�BR�BS�BR�BR�BX�BX�BY�BY�BZ�B[�B]B`Bb&Bd0Bf<BgBBhGBgDBgDBf;BhHBjVBpzBpxBr�Bs�Bu�Bz�B|�B�B��B�B�B� B�+B�*B�7B�FB�ZB�hB�}B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�#B�)B�@B�aB�jB�TB�lBȇB˗B̝BͣBѽB��B��B��B� B�B�B�7B�OB�[B�ZB�[B�SB�YB�lB�B��B��B	 �B	 �B	�B	�B	�B	�B	B		B	
B	B	/B	AB	OB	YB	]B	�B	"�B	&�B	(�B	*�B	+�B	-�B	/�B	/�B	/�B	/�B	1�B	5	B	6B	:,B	<5B	=:B	>AB	?IB	@QB	AVB	CaB	EoB	G{B	HB	I�B	K�B	N�B	R�B	S�B	T�B	U�B	V�B	X�B	Y�B	Z�B	\�B	]�B	_B	_B	aB	cB	cB	e)B	g5B	h<B	iDB	mZB	qtB	sB	t�B	v�B	y�B	{�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�0B	�CB	�B	�9B	��B	�NB	��B

B
VB
�B
)�B
2�B
9B
BVB
HxB
N�B
U�B
Z�B
bB
g2B
mVB
rvB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708302016053117083020160531170830  AO  ARCAADJP                                                                    20160330091600    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160330091600  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160330091600  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170830  IP                  G�O�G�O�G�O�                