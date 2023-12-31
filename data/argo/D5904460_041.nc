CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-30T19:18:47Z AOML 3.0 creation; 2016-08-07T21:17:35Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150330191847  20160807141735  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               )A   AO  5285_8895_041                   2C  D   APEX                            6487                            072314                          846 @�Ec���1   @�Ed� @.N��+�c���
=q1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    )A   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffBa��Bg��Bp  Bx  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�3D��D�VfD��fD��3D�fD�FfD��fD�� D�fD�P D�y�Dǹ�D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
>A	�A)�AI�Ai�A��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZ�Bc�HBi�HBrG�BzG�B�#�B�#�B�W
B��qB��B�#�B�#�B�#�B�#�B�#�B�W
B�W
B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE�{DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�{Dy��D�
D�h�D���D��pD�(�D�X�D���D��=D�(�D�b=D���D���D�ep1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�r�A�t�A�p�A�r�A�x�A�x�A�v�A�x�A�~�AӇ+AӇ+AӉ7AӉ7AӉ7AӋDAӋDAӉ7AӋDAӋDAӋDAӋDAӋDAӏ\Aӏ\Aӏ\AӋDA�&�A�ĜAЧ�A�z�A�+AάA���A�K�A�A�+Aț�A��A�p�A�9XA��HA��HA��^A��TA�oA�`BA�hsA�33A�1A���A��A�=qA�+A��A�`BA��A�{A���A��A��#A��A�v�A�ƨA��wA���A�M�A�ZA��A���A�-A�A���A��A�dZA��A|��Aw�wAqt�Am�7Aj=qAiAe`BAbr�Aa"�A]p�AY�TAY33AXr�AV��AVr�ASoAP��AP5?AO�-AO�AM;dAIl�AE�AC33A@bNA>A�A<��A:E�A9A8ZA6��A5��A4��A3�TA2-A/t�A.�A+��A*��A(�A(�DA(n�A'�A%�^A%+A$z�A"bNA!t�A ��Ap�A�AbNA�TAl�AG�A�;A�^Ax�A\)A33A�A�\A;dA��A��A�jA-A�PA33A�HA�AQ�A��A�yA��AI�A��A33A
=A��A7LA1'A^5A
  A	A�A�\A�+Ar�A��A�A�A/A�A�A�A��A9XA^5A=qAC�A��A/A�-Az�AZAA��A�;A�A\)A �A �yA�A ��A �`A ffA 1'A $�@�-@���@��j@��j@��@�^5@��@�A�@���@��w@�
=@�x�@���@���@��`@��;@�~�@��@�G�@�G�@�j@���@�P@�@�M�@��-@��^@��`@�Q�@�A�@�(�@��
@�S�@�R@�5?@�h@��@畁@�+@���@�V@�$�@���@�/@�(�@㕁@�33@���@�h@�^@�x�@��@�j@���@�r�@�l�@�V@���@��@ە�@ڧ�@�v�@�{@١�@��@؃@��
@�t�@�"�@ְ!@��@թ�@Ցh@�p�@�/@�I�@Ӯ@���@�$�@щ7@���@�  @�33@Ώ\@��@���@ͺ^@ͩ�@͑h@�X@̣�@�A�@��;@�+@�^5@ɩ�@�`B@�/@��@�Q�@� �@��;@ǥ�@�+@�
=@���@őh@�7L@���@ēu@�z�@�j@�1'@�ƨ@�+@�E�@�`B@��@��9@��@�S�@�o@��y@���@�ff@�5?@��@���@�/@�Z@��;@�dZ@���@�~�@�E�@�$�@��7@�V@��9@�Z@�9X@�b@�ƨ@�dZ@��@�v�@�E�@�{@��@��-@�hs@�O�@��@�Ĝ@���@�Q�@�  @��m@��F@�t�@�;d@��@��R@���@�~�@�=q@��@�`B@��@��j@�z�@� �@��@�ƨ@���@�l�@�K�@��y@���@���@�v�@�^5@�@�X@��`@��@�j@�b@��m@���@�dZ@�o@��\@��@�O�@���@��`@��D@�bN@�I�@�  @��
@��P@�dZ@�
=@���@���@��T@��7@�x�@�hs@�%@�r�@�A�@�  @���@�S�@��@�@�`B@���@���@��9@��u@��;@��@�dZ@�K�@��@��y@���@�E�@�{@���@�@�O�@�V@��`@���@���@�1'@�|�@�o@���@�^5@�@��-@��h@�p�@�/@���@���@��D@��D@�Z@�1'@�1@���@���@��P@�\)@�
=@��R@���@��+@�M�@��@���@�hs@�?}@�%@��9@��D@�r�@�A�@�ƨ@�t�@�+@���@�v�@�E�@�J@��T@�hs@��9@�  @�S�@��@��y@��R@��!@�^5@�E�@�bN@���@�J@z-@p�`@h�9@_�;@UO�@M��@DI�@=�-@8r�@1��@+331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�hsA�r�A�t�A�p�A�r�A�x�A�x�A�v�A�x�A�~�AӇ+AӇ+AӉ7AӉ7AӉ7AӋDAӋDAӉ7AӋDAӋDAӋDAӋDAӋDAӏ\Aӏ\Aӏ\AӋDA�&�A�ĜAЧ�A�z�A�+AάA���A�K�A�A�+Aț�A��A�p�A�9XA��HA��HA��^A��TA�oA�`BA�hsA�33A�1A���A��A�=qA�+A��A�`BA��A�{A���A��A��#A��A�v�A�ƨA��wA���A�M�A�ZA��A���A�-A�A���A��A�dZA��A|��Aw�wAqt�Am�7Aj=qAiAe`BAbr�Aa"�A]p�AY�TAY33AXr�AV��AVr�ASoAP��AP5?AO�-AO�AM;dAIl�AE�AC33A@bNA>A�A<��A:E�A9A8ZA6��A5��A4��A3�TA2-A/t�A.�A+��A*��A(�A(�DA(n�A'�A%�^A%+A$z�A"bNA!t�A ��Ap�A�AbNA�TAl�AG�A�;A�^Ax�A\)A33A�A�\A;dA��A��A�jA-A�PA33A�HA�AQ�A��A�yA��AI�A��A33A
=A��A7LA1'A^5A
  A	A�A�\A�+Ar�A��A�A�A/A�A�A�A��A9XA^5A=qAC�A��A/A�-Az�AZAA��A�;A�A\)A �A �yA�A ��A �`A ffA 1'A $�@�-@���@��j@��j@��@�^5@��@�A�@���@��w@�
=@�x�@���@���@��`@��;@�~�@��@�G�@�G�@�j@���@�P@�@�M�@��-@��^@��`@�Q�@�A�@�(�@��
@�S�@�R@�5?@�h@��@畁@�+@���@�V@�$�@���@�/@�(�@㕁@�33@���@�h@�^@�x�@��@�j@���@�r�@�l�@�V@���@��@ە�@ڧ�@�v�@�{@١�@��@؃@��
@�t�@�"�@ְ!@��@թ�@Ցh@�p�@�/@�I�@Ӯ@���@�$�@щ7@���@�  @�33@Ώ\@��@���@ͺ^@ͩ�@͑h@�X@̣�@�A�@��;@�+@�^5@ɩ�@�`B@�/@��@�Q�@� �@��;@ǥ�@�+@�
=@���@őh@�7L@���@ēu@�z�@�j@�1'@�ƨ@�+@�E�@�`B@��@��9@��@�S�@�o@��y@���@�ff@�5?@��@���@�/@�Z@��;@�dZ@���@�~�@�E�@�$�@��7@�V@��9@�Z@�9X@�b@�ƨ@�dZ@��@�v�@�E�@�{@��@��-@�hs@�O�@��@�Ĝ@���@�Q�@�  @��m@��F@�t�@�;d@��@��R@���@�~�@�=q@��@�`B@��@��j@�z�@� �@��@�ƨ@���@�l�@�K�@��y@���@���@�v�@�^5@�@�X@��`@��@�j@�b@��m@���@�dZ@�o@��\@��@�O�@���@��`@��D@�bN@�I�@�  @��
@��P@�dZ@�
=@���@���@��T@��7@�x�@�hs@�%@�r�@�A�@�  @���@�S�@��@�@�`B@���@���@��9@��u@��;@��@�dZ@�K�@��@��y@���@�E�@�{@���@�@�O�@�V@��`@���@���@�1'@�|�@�o@���@�^5@�@��-@��h@�p�@�/@���@���@��D@��D@�Z@�1'@�1@���@���@��P@�\)@�
=@��R@���@��+@�M�@��@���@�hs@�?}@�%@��9@��D@�r�@�A�@�ƨ@�t�@�+@���@�v�@�E�@�J@��T@�hs@��9@�  @�S�@��@��y@��R@��!@�^5G�O�@�bN@���@�J@z-@p�`@h�9@_�;@UO�@M��@DI�@=�-@8r�@1��@+331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�'B�-B�FB	B	�fB
F�B
��B
�jB
B
��B
��B
�fB
�B
��B
��BDB'�BB�BT�BiyB�B�B�bB��B�uB�\B�1B�{B��B�oB��B��B�!B�jB�XB�^B�3B��B�VB�B}�BYBC�B-BB
�
B
��B
ffB
M�B
-B
{B
	7B	�B	��B	��B	�=B	y�B	p�B	\)B	L�B	C�B	49B	"�B	�B	�B	�B	hB	1B	  B��B��B��B�B�mB�TB�BB�TB�fB�mB�B��B��B	B	B	+B	+B	PB	%�B	)�B	%�B	�B	�B	�B	�B	�B	�B	�B	)�B	)�B	%�B	#�B	'�B	'�B	'�B	'�B	'�B	&�B	%�B	+B	.B	1'B	49B	?}B	E�B	B�B	F�B	G�B	P�B	S�B	T�B	T�B	T�B	T�B	T�B	W
B	YB	\)B	gmB	gmB	e`B	k�B	n�B	w�B	q�B	e`B	VB	Q�B	N�B	M�B	M�B	L�B	J�B	G�B	G�B	G�B	H�B	YB	^5B	[#B	T�B	\)B	\)B	XB	R�B	Q�B	_;B	q�B	u�B	t�B	{�B	� B	~�B	�=B	�JB	�JB	�uB	��B	��B	��B	��B	�B	�B	�3B	�LB	B	ŢB	ŢB	ÖB	��B	B	ŢB	ĜB	ŢB	ŢB	ȴB	��B	ȴB	ǮB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�
B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�5B	�mB	�yB	�B	�B	�yB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
JB
JB
JB
JB
JB
JB
PB
JB
JB
JB
JB
VB
\B
bB
bB
\B
\B
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
 �B
"�B
"�B
+B
/B
5?B
9XB
A�B
G�B
M�B
Q�B
XB
]/B
`BB
e`B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B��B��B� B�B��B� B�B�
B�B�B�B�2B	B	�AB
F�B
��B
�?B
�bB
ʖB
лB
�:B
�xB
��B
��BB'�BBbBT�BiGB��B��B�.B�TB�BB�(B� B�HB�`B�8B�MB�`B��B�7B�%B�+B�B��B�%B��B}�BX�BC_B,�B�B
��B
�oB
f2B
M�B
,�B
HB
	B	�lB	ΨB	��B	�B	y�B	puB	[�B	L�B	ChB	4
B	"�B	�B	qB	RB	:B	B��B��B��B��B�qB�BB�)B�B�&B�8B�>B�B��B��B	 �B	�B	�B	�B	 B	%�B	)�B	%�B	�B	`B	uB	yB	B	_B	�B	)�B	)�B	%�B	#�B	'�B	'�B	'�B	'�B	'�B	&�B	%�B	*�B	-�B	0�B	4B	?GB	EmB	BYB	FpB	GwB	P�B	S�B	T�B	T�B	T�B	T�B	T�B	V�B	X�B	[�B	g6B	g3B	e'B	kLB	n_B	w�B	qqB	e'B	U�B	Q�B	N�B	M�B	M�B	L�B	J�B	GvB	GvB	GyB	H|B	X�B	]�B	Z�B	T�B	[�B	[�B	W�B	R�B	Q�B	_B	qnB	u�B	t�B	{�B	�B	~�B	�B	�B	�B	�;B	�_B	��B	��B	��B	��B	��B	��B	�B	�SB	�fB	�dB	�YB	�LB	�PB	�eB	�^B	�dB	�cB	�tB	ʁB	�uB	�oB	�aB	͕B	ҶB	ѭB	ѮB	ЧB	ΛB	̏B	̏B	ЦB	ЦB	ӶB	��B	��B	��B	��B	��B	��B	��B	ӸB	ЦB	ѬB	ҴB	ҴB	ҵB	ѬB	ҷB	ҶB	ѫB	ѬB	ӹB	ӸB	��B	��B	��B	��B	��B	�-B	�;B	�=B	�>B	�9B	�:B	�3B	�6B	�8B	�8B	�?B	�<B	�>B	�=B	�CB	�EB	�DB	�FB	�>B	�<B	�=B	�6B	�8B	�:B	�7B	�9B	�2B	�8B	�=B	�>B	�EB	�DB	�DB	�EB	�EB	�CB	�JB	�CB	�FB	�HB	�LB	�UB	�WB	�UB	�WB	�WB	�WB	�\B	�bB	�gB	�dB	�cB	�bB	�\B	�]B	�eB	�gB	�fB	�gB	�eB	�lB	�tB	�xB	�xB	�zB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
B
B
B
	B
B
B
	B
B
B
B
B
B
B
B
B
B
!B
B
B
B
B
$B
#B
"B
#B
#B
$B
,B
+B
+B
*B
4B
2B
/B
1B
2B
7B
5B
4B
5B
>B
>B
BB
BB
CB
DB
IB
JB
JB
JB
KB
JB
GB
PB
PB
PB
QB
VB
XB
[B
]B
\B
[B
dB
aB
nB
rB
yB
{B
 �B
 �B
{B
yB
}B
 �B
!�B
!�B
!�B
!�B
"�B
!�B
 �B
{B
{B
{B
yB
yB
 �G�O�B
"�B
*�B
.�B
4�B
9B
ACB
GiB
M�B
Q�B
W�B
\�B
_�B
eB
k>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417352016080714173520160807141735  AO  ARCAADJP                                                                    20150330191847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150330191847  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150330191847  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141735  IP                  G�O�G�O�G�O�                