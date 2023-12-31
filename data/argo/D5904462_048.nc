CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-24T02:17:39Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150424021739  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               0A   AO  5287_9017_048                   2C  D   APEX                            6529                            072314                          846 @�Kf��1   @�Kf�� @13t�j~��d�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    0A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B:  B?��BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyY�D�fD�I�D��3D��3D���D�L�D���D���D�#3D�S3D�y�D���D�	�D�FfDڌ�D��fD�fD�I�D�c3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ϮA�
A'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B;��BA�]BI�]BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qC}qCc�C}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^}qC`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�K�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�1�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dyx�D�D�YHD���D���D�{D�\{D��{D��{D�2�D�b�D��HD��{D�HD�VDڜ{D��D�D�YHD�r�D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�G�A�G�A�1'A�$�A�oA��yA�A͕�A�p�A�K�A�K�A�\)A�ffA�hsA�n�A�l�A�l�A�n�A�p�A�n�A�VA�dZA�1'AƑhA�"�A�ƨA�ȴA�S�A�1A�A�;dA��A�9XA���A���A�t�A�1A�jA�ƨA��\A��PA�1A�1A���A���A�?}A�XA��`A�`BA�33A��A�|�A���A�p�A�JA�ĜA��PA��/A���A��A�oA���A���A�{A��PA��A�;dA��A�VA��#A�E�A�~�A��+A���A���A�oA�x�A�&�A�z�A�O�A��A��A�A��A�/A�33A��-A�A�~�A�dZA��A���A�dZA��uA���A�7LA|�Axz�AwC�AvffAu�7Atn�At9XAs��Ao&�AmoAlr�Ak��Ak�Ajn�AiC�Ad~�Ab9XAb�Aa�A^��A\�DAXA�AU"�AQ�AO�
ANz�AM|�AK?}AI�mAH�DAF�!AEC�AD��ADI�AC`BABA@n�A>ĜA<��A;p�A:��A:-A9�^A8z�A6�DA5?}A4��A2Q�A/K�A-�A,�`A,ffA+�mA*M�A)x�A)�A(��A(A�A&I�A$n�A#`BA#
=A"�jA!�TA ��A�hA33A��AffA�
A"�A\)A�AM�A�;A�7AK�A�A��A-A�;A33A�AC�A�A�Ax�A%AE�AA?}A��A~�A��A
A�A	�Az�A�#A�-AXA��A �AJAA�AVA5?A9XA9XA��A��A�FAp�A33AXA��AC�A �A=qA��AI�A��AO�A�AM�A�AJA��A �A  �@��@���@���@�%@�$�@��;@��P@���@��@�I�@���@�"�@���@��h@��+@��@�hs@��@��`@�{@�F@�J@��y@웦@�33@�X@�F@��`@㝲@�\@�^@�@�x�@��/@��/@ߝ�@�t�@�l�@��H@��@���@�X@�S�@���@��/@�\)@ѩ�@�@�5?@�^5@�@�@���@мj@Ͼw@��@Η�@�ff@�V@�-@ͩ�@�G�@���@�A�@��
@˾w@˝�@�\)@���@�@�O�@�Ĝ@�1'@�o@Ƨ�@��@ƸR@Ƈ+@�E�@š�@�/@�j@�b@��;@þw@�|�@�C�@�
=@°!@�E�@��^@�O�@���@�Ĝ@��m@��F@���@���@�t�@�l�@�\)@�S�@���@���@�O�@�z�@�ƨ@���@�@�hs@���@��9@��D@��;@���@�S�@��H@�v�@��@���@�`B@��D@�A�@�  @��F@�C�@�n�@�{@���@�@��^@���@���@��7@�?}@���@��@�z�@�r�@� �@�S�@��@��!@��+@��@���@�G�@�7L@�V@���@���@���@��@���@��9@���@�j@�(�@��@�b@��@��m@���@�\)@�;d@���@�n�@��#@��^@�X@��j@�r�@�Z@�Z@�I�@��;@�S�@���@��!@��\@�v�@�^5@�M�@��-@���@���@�j@��;@�ƨ@�ƨ@���@�o@�V@�-@��@��@��T@�p�@�b@�t�@�;d@�ȴ@�^5@�@��@���@���@���@���@�Ĝ@���@�A�@���@��@���@��H@��R@�~�@�5?@�J@��#@�x�@�G�@�&�@��@���@�r�@�  @��P@�l�@�S�@�
=@��R@�-@�@���@��@��T@���@��-@�O�@�/@�V@��u@�r�@�Q�@��;@�l�@�+@�"�@�o@���@��@���@��R@��+@�ff@�J@��^@�x�@�G�@��@�ȴ@�`B@�t�@|�j@t9X@lI�@e�@[��@R�H@I7L@@�9@:��@6@/
=@(bN@"~�@Z@��@33@bN@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�G�A�G�A�G�A�1'A�$�A�oA��yA�A͕�A�p�A�K�A�K�A�\)A�ffA�hsA�n�A�l�A�l�A�n�A�p�A�n�A�VA�dZA�1'AƑhA�"�A�ƨA�ȴA�S�A�1A�A�;dA��A�9XA���A���A�t�A�1A�jA�ƨA��\A��PA�1A�1A���A���A�?}A�XA��`A�`BA�33A��A�|�A���A�p�A�JA�ĜA��PA��/A���A��A�oA���A���A�{A��PA��A�;dA��A�VA��#A�E�A�~�A��+A���A���A�oA�x�A�&�A�z�A�O�A��A��A�A��A�/A�33A��-A�A�~�A�dZA��A���A�dZA��uA���A�7LA|�Axz�AwC�AvffAu�7Atn�At9XAs��Ao&�AmoAlr�Ak��Ak�Ajn�AiC�Ad~�Ab9XAb�Aa�A^��A\�DAXA�AU"�AQ�AO�
ANz�AM|�AK?}AI�mAH�DAF�!AEC�AD��ADI�AC`BABA@n�A>ĜA<��A;p�A:��A:-A9�^A8z�A6�DA5?}A4��A2Q�A/K�A-�A,�`A,ffA+�mA*M�A)x�A)�A(��A(A�A&I�A$n�A#`BA#
=A"�jA!�TA ��A�hA33A��AffA�
A"�A\)A�AM�A�;A�7AK�A�A��A-A�;A33A�AC�A�A�Ax�A%AE�AA?}A��A~�A��A
A�A	�Az�A�#A�-AXA��A �AJAA�AVA5?A9XA9XA��A��A�FAp�A33AXA��AC�A �A=qA��AI�A��AO�A�AM�A�AJA��A �A  �@��@���@���@�%@�$�@��;@��P@���@��@�I�@���@�"�@���@��h@��+@��@�hs@��@��`@�{@�F@�J@��y@웦@�33@�X@�F@��`@㝲@�\@�^@�@�x�@��/@��/@ߝ�@�t�@�l�@��H@��@���@�X@�S�@���@��/@�\)@ѩ�@�@�5?@�^5@�@�@���@мj@Ͼw@��@Η�@�ff@�V@�-@ͩ�@�G�@���@�A�@��
@˾w@˝�@�\)@���@�@�O�@�Ĝ@�1'@�o@Ƨ�@��@ƸR@Ƈ+@�E�@š�@�/@�j@�b@��;@þw@�|�@�C�@�
=@°!@�E�@��^@�O�@���@�Ĝ@��m@��F@���@���@�t�@�l�@�\)@�S�@���@���@�O�@�z�@�ƨ@���@�@�hs@���@��9@��D@��;@���@�S�@��H@�v�@��@���@�`B@��D@�A�@�  @��F@�C�@�n�@�{@���@�@��^@���@���@��7@�?}@���@��@�z�@�r�@� �@�S�@��@��!@��+@��@���@�G�@�7L@�V@���@���@���@��@���@��9@���@�j@�(�@��@�b@��@��m@���@�\)@�;d@���@�n�@��#@��^@�X@��j@�r�@�Z@�Z@�I�@��;@�S�@���@��!@��\@�v�@�^5@�M�@��-@���@���@�j@��;@�ƨ@�ƨ@���@�o@�V@�-@��@��@��T@�p�@�b@�t�@�;d@�ȴ@�^5@�@��@���@���@���@���@�Ĝ@���@�A�@���@��@���@��H@��R@�~�@�5?@�J@��#@�x�@�G�@�&�@��@���@�r�@�  @��P@�l�@�S�@�
=@��R@�-@�@���@��@��T@���@��-@�O�@�/@�V@��u@�r�@�Q�@��;@�l�@�+@�"�@�o@���@��@���@��R@��+@�ff@�J@��^@�x�@�G�G�O�@�ȴ@�`B@�t�@|�j@t9X@lI�@e�@[��@R�H@I7L@@�9@:��@6@/
=@(bN@"~�@Z@��@33@bN@�/11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BH�BG�BF�BD�BB�B@�B@�BF�BL�BQ�BW
B`BBe`Bp�B�B�PB�hB�oB�uB��B��B��B��B��BVB�B�B�B"�B/B5?B-B�B.BJ�BT�BS�BW
B]/B]/BYBT�BXBS�BB�B@�B?}B;dB+B�BB��B�yB�fB�
BƨB�FB��B��B��Bs�BT�BA�B0!BDB�B�BȴB�wB�B��B��B�7B}�BiyB]/BS�BaHB`BBM�BA�B8RB1'B"�B�B
��B
�HB
�B
��B
�LB
��B
��B
�bB
� B
^5B
Q�B
49B
�B
uB
\B
	7B
+B
oB
oB	��B	�B	�ZB	�5B	�B	��B	ŢB	��B	�PB	�DB	�%B	�B	u�B	_;B	M�B	:^B	/B	%�B	�B	oB	DB	1B		7B	B	B	B	B	B	B��B�B�B�B�B�mB�NB�B��B��BȴBÖBB��B��B�}BÖBĜBŢBƨBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBƨB��B�HB�/B�)B�;B�;B�/B�NB�TB�NB�NB�TB�mB�B�B��B��B��B��B��B��B��B��B��B	B	B		7B	JB	!�B	!�B	#�B	)�B	-B	,B	(�B	1'B	I�B	cTB	e`B	dZB	dZB	cTB	dZB	dZB	e`B	ffB	ffB	hsB	k�B	w�B	w�B	r�B	m�B	l�B	m�B	n�B	r�B	z�B	z�B	~�B	� B	�JB	�DB	�1B	�%B	�=B	�uB	�VB	�1B	|�B	u�B	p�B	k�B	e`B	]/B	XB	S�B	ZB	cTB	cTB	cTB	gmB	ffB	ffB	gmB	gmB	`BB	XB	S�B	M�B	M�B	I�B	F�B	E�B	L�B	S�B	YB	XB	XB	W
B	YB	XB	YB	ZB	ZB	[#B	\)B	]/B	\)B	\)B	^5B	_;B	`BB	`BB	`BB	`BB	aHB	e`B	jB	iyB	iyB	jB	n�B	o�B	o�B	o�B	o�B	p�B	q�B	s�B	t�B	u�B	w�B	y�B	z�B	{�B	}�B	� B	�B	�B	�B	�PB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�3B	�9B	�LB	�LB	�XB	�dB	�jB	�wB	�}B	��B	��B	��B	��B	B	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�/B	�5B	�5B	�BB	�TB	�ZB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
\B
�B
"�B
'�B
,B
5?B
8RB
=qB
D�B
L�B
P�B
W
B
_;B
e`B
jB
n�B
r�B
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BH�BH�BG�BF~BDtBBfB@VB@XBF}BL�BQ�BV�B`Be5BpxB��B�+B�>B�IB�PB�\B�jB��BΰB��B/BlB�B�B"�B.�B5B,�B�B-�BJ�BT�BS�BV�B]	B]
BX�BT�BW�BS�BBiB@\B?QB;9B*�B�B�B��B�NB�<B��B�|B�B��B��B�YBs�BT�BAYB/�BB�]B��BȆB�EB��B��B�UB�B}�BiKB\�BS�BaB`BM�BAZB8$B0�B"�BRB
��B
�B
��B
˙B
�B
��B
��B
�4B
�B
^B
Q�B
4B
pB
GB
1B
	B
B
DB
AB	��B	�[B	�1B	�B	��B	��B	�zB	��B	�*B	�B	��B	��B	u�B	_B	M�B	:6B	.�B	%�B	�B	JB	B	B		B	�B	�B	 �B	 �B	�B	 �B��B�B�zB�dB�[B�IB�,B��B��B̨BȑB�rB�jB�eB�^B�WB�rB�wB�|BƂB�~BȏBͬBϷB��B��B��BϸBпBпBϺBϹBαBϹBпB��BпBϸB϶BϷBαB̦BʛBɓBƀBβB�!B�B� B�B�B�B�'B�+B�$B�$B�*B�EB�VB�uB��B��B��B��B�B��B��B��B��B	 �B	�B		B	B	!�B	!�B	#�B	)�B	,�B	+�B	(�B	0�B	I�B	c$B	e2B	d)B	d,B	c$B	d(B	d+B	e/B	f5B	f5B	hDB	kUB	w�B	w�B	r�B	maB	lZB	maB	nhB	rB	z�B	z�B	~�B	�B	�B	�B	� B	��B	�B	�BB	�$B	��B	|�B	u�B	psB	kTB	e.B	\�B	W�B	S�B	Y�B	c$B	c#B	c!B	g:B	f3B	f3B	g:B	g:B	`B	W�B	S�B	M�B	M�B	I�B	FvB	EsB	L�B	S�B	X�B	W�B	W�B	V�B	X�B	W�B	X�B	Y�B	Y�B	Z�B	[�B	\�B	[�B	[�B	^B	_
B	`B	`B	`B	`B	aB	e.B	jKB	iFB	iEB	jMB	nfB	okB	okB	okB	okB	pqB	qtB	s�B	t�B	u�B	w�B	y�B	z�B	{�B	}�B	�B	��B	��B	��B	�B	�3B	�?B	�?B	�RB	�^B	�eB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�.B	�3B	�BB	�FB	�KB	�LB	�RB	�VB	�YB	�fB	�jB	�pB	�sB	�wB	�wB	�wB	�xB	ɂB	ʋB	˒B	̖B	̖B	͝B	ϩB	ҿB	ҼB	ҺB	һB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�
B	�B	�"B	�B	�"B	�(B	�(B	�'B	�/B	�-B	�-B	�0B	�-B	�.B	�)B	�&B	�-B	�;B	�FB	�FB	�FB	�MB	�TB	�SB	�_B	�`B	�^B	�sB	�tB	�~B	�}B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�~B	�|B	�B	�}B	�B	�~B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
 �B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�G�O�B
�B
"B
fB
"�B
'�B
+�B
5B
8B
=6B
DcB
L�B
P�B
V�B
_B
e%B
jEB
n[B
rtB
u�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150424021739    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150424021739  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150424021739  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                