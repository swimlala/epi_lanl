CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-25T09:15:45Z AOML 3.0 creation; 2016-08-07T21:51:22Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151025091545  20160807145123  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               SA   AO  5287_9017_083                   2C  D   APEX                            6529                            072314                          846 @�y��2u�1   @�y�\�(�@0����S��d�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    SA   B   B   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�33B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�3D�S3D�c3D��3D���D�9�D�|�D��fD�	�D�@ D�� D��3D���D�fDړ3D���D���D�@ D�fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�z�A=qA'�
AG�
Ag�
A��A��A��A��A��A��A��A��B��B	��B��B��B!��B)��B1��B9��BA��BI��BR\)BY��Ba��Bi��Bq��By��B���B���B���B���B���B�.B�aGB���B���B���B���B���B���B�.B���B���B�ǮB�ǮB���B���B�.B�aGB���B���B���B���B���B���B�.B�ǮB���B���C }qC}qC}qC}qC}qC
}qC}qC}qC}qC}qC}qC}qC}qCc�C}qC}qC }qC"}qC$}qC&}qC(}qC*}qC,}qC.}qC0}qC2}qC4}qC6}qC8}qC:}qC<}qC>}qC@}qCB}qCD}qCF}qCH}qCJ}qCL}qCN}qCP}qCR}qCT}qCV}qCX}qCZ}qC\}qC^�C`}qCb}qCd}qCf}qCh}qCj}qCl}qCn}qCp}qCr}qCt}qCv}qCx}qCz}qC|}qC~}qC�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�C�>�D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[��D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du)Dy�\D��D�b�D�r�D���D�{D�IHD��{D��D�HD�O�D���D���D�	HD�Dڢ�D��{D�	HD�O�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A��A���A��A��HA�^A�A��A�!A�!A�!A�-A�!A�A�A�A�A�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A㟾A�|�A���A�
=AمA��HA־wA�~�A�bNA�/A���AӋDA�oA��;A�x�A���A�
=A�M�A�1'A�(�A�ffA���A���A�A�A��/A�O�A�5?A�K�A��uA�(�A��A�G�A�~�A��A�/A�XA��FA�A��A�C�A��A��mA�A��HA�ffA��A��-A��A�bNA���A��A��FA��A��mA���A���A��DA�M�A�ffA��A�hsA�5?A��A�K�A�S�A�l�A�ƨA��FA���A��FA��;A��PA��A��A}p�A|=qAyt�AtI�Aq��Ao��An~�Am"�Ak��Ajz�AhbAap�A]l�AY�;AW�#AV�jARȴAP9XAO�AM��AJ  AF��AD��A@��A=l�A;|�A9�
A81A7��A7x�A7dZA7G�A7;dA6�!A6�\A6~�A6VA61A4��A2�/A0�A/dZA-/A*�yA+C�A*��A(I�A'7LA&�yA&�A&n�A#O�A!
=A!%A#�A%`BA$�A#�PA"I�A!S�A ��AVA��A�mA�PA/A��A�
AZA��A�^A7LA��A��A�Az�A�AȴA~�AA  A1A&�AM�A�A  A��AffAXA
�A	�-A�mA�A�FA�A$�AhsA��AA �AE�A1'A��A+A�AjA33A&�A�A�DA��A�`A�!AI�A/A bNA M�A  �@��@���@�
=@�?}@��@��;@�n�@���@��`@�(�@���@��@��#@�p�@��/@��
@�@�@�J@���@���@�n�@�h@�D@�w@�;d@��@�~�@��#@�^@��@���@�F@��@�;d@�+@�$�@�?}@� �@��@��@��
@�K�@��@�ff@�M�@�=q@�~�@�~�@�-@���@�@�/@�j@�dZ@��T@�`B@��m@�C�@�5?@ّh@�hs@ؼj@��y@�~�@�{@ՙ�@�`B@ԛ�@�ƨ@���@��@�@�`B@��`@�(�@�v�@͑h@�&�@�%@��@��@��@�v�@��@�`B@��`@���@�Ĝ@ȓu@ȣ�@�Z@�Q�@�r�@���@�r�@Ƨ�@�ff@�{@Ų-@�x�@ŉ7@��@�j@� �@�|�@�
=@��H@�ȴ@§�@�=q@��^@�%@�r�@��;@�dZ@�
=@��R@�=q@���@�7L@�Q�@� �@�t�@�o@��H@��!@��+@��@�?}@���@��u@�b@��@�|�@�C�@���@��@���@�M�@�{@�hs@���@�(�@�t�@��@��\@���@�?}@�%@���@�1@���@��y@�O�@�Ĝ@� �@��m@�S�@�"�@�
=@��H@���@�n�@�E�@�{@�@��@��^@�@��-@��7@��/@�(�@��w@���@�"�@�ȴ@���@�~�@�n�@�E�@�5?@�@���@��h@���@��@�r�@�1'@�1'@���@��m@��m@���@�33@���@�~�@�5?@�$�@���@��#@�@��^@�hs@�A�@�1@���@��m@��w@��@�33@���@�5?@�{@��@���@���@�G�@��/@���@�z�@�I�@��@�dZ@�33@�"�@��y@���@�-@���@��h@��`@��D@�A�@� �@�  @��F@��@���@���@�~�@�E�@�$�@��@��T@��^@��7@�X@�/@��@��@��u@�Z@��w@�|�@�"�@���@���@�~�@�^5@�V@��m@�v�@�`B@��@w�@nv�@e�-@\�@T��@J��@@�9@8�`@2~�@+��@%��@ ��@�@��@��@�`@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�"�A��A���A��A��HA�^A�A��A�!A�!A�!A�-A�!A�A�A�A�A�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A㟾A�|�A���A�
=AمA��HA־wA�~�A�bNA�/A���AӋDA�oA��;A�x�A���A�
=A�M�A�1'A�(�A�ffA���A���A�A�A��/A�O�A�5?A�K�A��uA�(�A��A�G�A�~�A��A�/A�XA��FA�A��A�C�A��A��mA�A��HA�ffA��A��-A��A�bNA���A��A��FA��A��mA���A���A��DA�M�A�ffA��A�hsA�5?A��A�K�A�S�A�l�A�ƨA��FA���A��FA��;A��PA��A��A}p�A|=qAyt�AtI�Aq��Ao��An~�Am"�Ak��Ajz�AhbAap�A]l�AY�;AW�#AV�jARȴAP9XAO�AM��AJ  AF��AD��A@��A=l�A;|�A9�
A81A7��A7x�A7dZA7G�A7;dA6�!A6�\A6~�A6VA61A4��A2�/A0�A/dZA-/A*�yA+C�A*��A(I�A'7LA&�yA&�A&n�A#O�A!
=A!%A#�A%`BA$�A#�PA"I�A!S�A ��AVA��A�mA�PA/A��A�
AZA��A�^A7LA��A��A�Az�A�AȴA~�AA  A1A&�AM�A�A  A��AffAXA
�A	�-A�mA�A�FA�A$�AhsA��AA �AE�A1'A��A+A�AjA33A&�A�A�DA��A�`A�!AI�A/A bNA M�A  �@��@���@�
=@�?}@��@��;@�n�@���@��`@�(�@���@��@��#@�p�@��/@��
@�@�@�J@���@���@�n�@�h@�D@�w@�;d@��@�~�@��#@�^@��@���@�F@��@�;d@�+@�$�@�?}@� �@��@��@��
@�K�@��@�ff@�M�@�=q@�~�@�~�@�-@���@�@�/@�j@�dZ@��T@�`B@��m@�C�@�5?@ّh@�hs@ؼj@��y@�~�@�{@ՙ�@�`B@ԛ�@�ƨ@���@��@�@�`B@��`@�(�@�v�@͑h@�&�@�%@��@��@��@�v�@��@�`B@��`@���@�Ĝ@ȓu@ȣ�@�Z@�Q�@�r�@���@�r�@Ƨ�@�ff@�{@Ų-@�x�@ŉ7@��@�j@� �@�|�@�
=@��H@�ȴ@§�@�=q@��^@�%@�r�@��;@�dZ@�
=@��R@�=q@���@�7L@�Q�@� �@�t�@�o@��H@��!@��+@��@�?}@���@��u@�b@��@�|�@�C�@���@��@���@�M�@�{@�hs@���@�(�@�t�@��@��\@���@�?}@�%@���@�1@���@��y@�O�@�Ĝ@� �@��m@�S�@�"�@�
=@��H@���@�n�@�E�@�{@�@��@��^@�@��-@��7@��/@�(�@��w@���@�"�@�ȴ@���@�~�@�n�@�E�@�5?@�@���@��h@���@��@�r�@�1'@�1'@���@��m@��m@���@�33@���@�~�@�5?@�$�@���@��#@�@��^@�hs@�A�@�1@���@��m@��w@��@�33@���@�5?@�{@��@���@���@�G�@��/@���@�z�@�I�@��@�dZ@�33@�"�@��y@���@�-@���@��h@��`@��D@�A�@� �@�  @��F@��@���@���@�~�@�E�@�$�@��@��T@��^@��7@�X@�/@��@��@��u@�Z@��w@�|�@�"�@���@���@�~�@�^5G�O�@��m@�v�@�`B@��@w�@nv�@e�-@\�@T��@J��@@�9@8�`@2~�@+��@%��@ ��@�@��@��@�`@9X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ZB
YB
VB
Q�B
O�B
O�B
O�B
P�B
R�B
T�B
VB
VB
W
B
W
B
VB
W
B
W
B
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
]/B
m�B
�=B
��B
��B
��B
�B
��B
=BhBuB<jBR�BgmB��BȴB�HB��B�B�B�B"�B/B33BdZB��B��B�qBB��B��B��BĜB�?B�B��B�uBn�BJ�BW
BK�B6FB
=B��B�FB��Bz�B�B� Bs�BdZBVBI�B9XB,B�B1B1B	7BB
��B
��B
�B
�BB
��B
�B
��B
�\B
�B
u�B
]/B
A�B
%�B
�B
PB	��B	�;B	��B	ŢB	�jB	�3B	��B	��B	�JB	iyB	T�B	D�B	9XB	1'B	!�B	�B	bB		7B��B�B�B�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B�TB��B	B��B�B�B�yB�fB��BĜB��B	1B	+B	)�B	$�B	�B	�B	VB�B��B��B��B��B��B��B�B�yB	B	J�B	VB	q�B	�B	�{B	�3B	�LB	�XB	�3B	�{B	�B	v�B	m�B	aHB	XB	N�B	E�B	A�B	@�B	8RB	33B	33B	8RB	6FB	2-B	33B	7LB	>wB	N�B	S�B	T�B	S�B	T�B	T�B	T�B	R�B	S�B	R�B	R�B	jB	v�B	x�B	x�B	{�B	|�B	}�B	� B	�B	�%B	�1B	�JB	�DB	�PB	�VB	�JB	�=B	�DB	�PB	�DB	�\B	�{B	�uB	�hB	�uB	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�jB	�jB	�qB	�qB	�jB	�dB	�jB	�qB	��B	�qB	�qB	�dB	�^B	�^B	�^B	�RB	�LB	�XB	�XB	�XB	�LB	�?B	�3B	�-B	�'B	�'B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�9B	�FB	�LB	�XB	�qB	��B	B	ȴB	ƨB	��B	��B	��B	��B	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�HB	�BB	�BB	�BB	�HB	�HB	�TB	�TB	�NB	�NB	�HB	�HB	�BB	�HB	�TB	�HB	�BB	�HB	�NB	�TB	�ZB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B

=B
DB
DB
DB
DB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
hB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
,B
2-B
9XB
@�B
H�B
M�B
T�B
ZB
`BB
e`B
jB
m�B
q�B
u�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
Z B
X�B
U�B
Q�B
O�B
O�B
O�B
P�B
R�B
T�B
U�B
U�B
V�B
V�B
U�B
V�B
V�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Y�B
[B
[B
[B
[B
[B
\	B
\	B
]B
]B
]B
]B
^B
]B
mqB
�B
��B
��B
ϽB
�uB
��B
BABQB<EBR�BgIB�bBȎB�B��B_BzB�B"�B.�B3
Bd0B�TB��B�HB�jB̣BάBͬB�sB�B��B��B�IBnoBJ�BV�BK�B6B
BʔB�B��Bz�B��B�Bs�Bd.BU�BI�B9-B+�B�BBB	B�B
��B
��B
�kB
�B
˚B
��B
��B
�3B
��B
u�B
]B
A\B
%�B
\B
&B	��B	�B	϶B	�wB	�BB	�
B	��B	��B	�!B	iSB	T�B	DwB	91B	1B	!�B	aB	=B		B��B�B�fB�OB�fB�rB�qB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�jB�/B��B	 �B��B�qB�]B�RB�BB��B�vBβB	B	*�B	)�B	$�B	�B	]B	/B�B��B��B϶B��B��B��B��B�QB	�B	J�B	U�B	q|B	��B	�OB	�B	�B	�(B	�B	�MB	��B	v�B	mfB	aB	W�B	N�B	EuB	A\B	@UB	8"B	3B	3B	8#B	6B	1�B	3B	7B	>HB	N�B	S�B	T�B	S�B	T�B	T�B	T�B	R�B	S�B	R�B	R�B	jQB	v�B	x�B	x�B	{�B	|�B	}�B	�B	��B	��B	�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�)B	�JB	�EB	�4B	�EB	�`B	�iB	�\B	�JB	�HB	�TB	�`B	�jB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�6B	�6B	�=B	�<B	�6B	�3B	�7B	�;B	�MB	�;B	�=B	�1B	�)B	�+B	�*B	�B	�B	� B	�"B	�#B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�9B	�SB	�ZB	�B	�sB	�VB	�TB	�SB	�TB	�_B	�dB	�pB	�qB	�rB	�yB	�~B	�}B	�}B	ȀB	�}B	ɃB	ɅB	̗B	̗B	ΣB	ΥB	ϦB	ЯB	ѵB	ЯB	ЮB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�!B	�&B	�"B	�'B	�(B	�6B	�4B	�<B	�:B	�BB	�EB	�HB	�MB	�_B	�_B	�fB	�fB	�jB	�lB	�jB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
B
B
B
B
B
"B
"B
#B
%B
'B
*B
,B
0B
-B
7B
<B
8B
?B
FB
GB
IB
NG�O�B
NB
QB
eB
 �B
$�B
+�B
1�B
9B
@JB
HzB
M�B
T�B
Y�B
`B
e%B
jBB
mWB
qlB
u�B
z�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.49 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451232016080714512320160807145123  AO  ARCAADJP                                                                    20151025091545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151025091545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151025091545  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145123  IP                  G�O�G�O�G�O�                