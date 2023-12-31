CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-03T02:16:23Z AOML 3.0 creation; 2016-08-07T21:17:38Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150703021623  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ;A   AO  5285_8895_059                   2C  D   APEX                            6487                            072314                          846 @�\��_�1   @�\�@y@@,=�E���c��7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ;A   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB���B���B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D�  D�\�D�y�D���D� D�@ D���D��fD��3D�@ D�� Dǰ D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�
>A	�A)�AI�Ai�A��\A��\A��\A��\Aď\Aԏ\A�\A�\BG�B
G�BG�BG�B"G�B*G�B2G�B:G�BBG�BJG�BRG�BZG�BbG�BjG�BrG�Bz�B��=B��qB��B�#�B��qB�#�B�#�B�#�B�#�B�W
B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�U�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�C�H�D ${D �{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D	${D	�{D
${D
�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D${D�{D ${D �{D!${D!�{D"${D"�{D#${D#�{D$${D$�{D%${D%�{D&${D&�{D'${D'�{D(${D(�{D)${D)�{D*${D*�{D+${D+�{D,${D,�{D-${D-�{D.${D.�{D/${D/�{D0${D0�{D1${D1�{D2${D2�{D3${D3�{D4${D4�{D5${D5�{D6${D6�{D7${D7�{D8${D8�{D9${D9�{D:${D:�{D;${D;�{D<${D<�{D=${D=�{D>${D>�{D?${D?�{D@${D@�{DA${DA�{DB${DB�{DC${DC�{DD${DD�{DE${DE��DF${DF�{DG${DG�{DH${DH�{DI${DI�{DJ${DJ�{DK${DK�{DL${DL�{DM${DM�{DN${DN�{DO${DO�{DP${DP�{DQ${DQ�{DR${DR�{DS${DS�{DT${DT�{DU${DU�{DV${DV�{DW${DW�{DX${DX�{DY${DY�{DZ${DZ�{D[${D[�{D\${D\�{D]${D]�{D^${D^�{D_${D_�{D`${D`�{Da${Da�{Db${Db�{Dc${Dc�{Dd${Dd�{De${De�{Df${Df�{Dg${Dg�{Dh${Dh�{Di${Di�{Dj${Dj�{Dk${Dk�{Dl${Dl�{Dm${Dm�{Dn${Dn�{Do${Do�{Dp${Dp�{Dq${Dq�{Dr${Dr�{Ds${Ds�{Dt${Dt�Dy��D�=D�o
D���D��
D�"=D�R=D���D��D�pD�R=D��=D��=D�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�K�A�K�A�I�A�C�A�I�A�A�A�?}A�7LA�33A�1'A�$�A�{A�VA��Aڕ�AًDA��/A؏\A�dZA�Q�A�G�A�?}A�9XA� �A�oA���A׺^Aק�Aם�A�z�A��yAվwA�O�A��A�9XA��`A�jA�ƨA�ZA��A��A�A�(�A�^5A�\)A��A���A��TA��PA��HA��`A�Q�A�^5A�E�A��;A�{A���A�l�A���A�A�
=A�-A��A�\)A�;dA�{A�9XA���A�r�A���A�A��uA�?}A� �A���A�z�A���A�  A��A�%A��DA���A�`BA�5?A�A{�Ax~�Aq�Am��Aj �Ag&�Ab��A_�A[�AUC�AQ�AK��AI�AF��AD��AA��A>ȴA<�A9�hA7�hA6ȴA5��A3�A1��A/�A/O�A/�A.bNA-
=A,JA+�mA,bA,^5A,��A-dZA-��A,��A,bNA,r�A+hsA*E�A( �A'oA&�A$ZA#�A"�!A"A!��A!�FA!`BA I�A�hAS�AC�A�hAC�A{AO�Ar�AZAr�A�+AO�AA�A9XA�7A"�AA�uAJA��A�A��AƨA�PA33A�A�PA��A�`AJAJA�wA`BA7LA�A�A�^A|�A�A�/An�A  A�A&�A�A�DAM�A�wAK�AA��A��AZA$�AAp�AK�A7LAv�A�mAK�A��Az�AffAA�A �A��A`BA
��A
Q�A
A�A
1A	A	x�A	;dA	%A��AQ�A1'A=qA(�A�mA��A&�A�A5?A��A;dA
=A�/A��A��A��A?}AVA�A��AĜA�\AbA�FAx�A �A �RA �\A ~�A bNA -A A @���@�|�@��H@��@��7@�`B@�X@��@�Q�@��R@��@��@���@��@�t�@��@���@�V@���@��9@�  @�S�@�n�@�-@�X@��D@�b@�\)@���@�$�@���@웦@�bN@��
@�o@�~�@�@�V@�r�@� �@�@��@�^@�G�@�Ĝ@�r�@��@㕁@�o@�\@�h@�Z@��@߮@�@�J@�`B@�V@��@܃@��@�|�@ڗ�@�$�@��#@�Ĝ@�(�@���@��@�^5@ղ-@�&�@��/@�Z@Ӿw@�S�@җ�@�E�@�@�@�V@Гu@��@�33@���@��@́@�%@̋D@�Z@�b@�\)@��y@ʇ+@�V@�5?@ɉ7@��@�9X@�1@��;@��
@�t�@�@Ƈ+@�M�@��#@ř�@�p�@�7L@ě�@�I�@�1@��
@Õ�@�S�@¸R@�J@���@�Z@��@�ƨ@���@�dZ@�;d@�+@�o@��@�~�@�5?@��@���@��j@�1@�ƨ@���@���@��P@�;d@��@���@���@��@�I�@���@�E�@�5?@��@���@��@�@��h@��@�z�@�r�@�j@��@�ƨ@�C�@�ȴ@��#@��@�bN@�1@���@�|�@�"�@��\@�p�@�9X@��@��@���@�+@��\@�=q@�`B@��@��`@���@�Z@���@�ȴ@�V@�=q@��#@���@��u@�I�@�(�@��@��@�+@��\@�E�@��@�J@��^@�O�@��@��u@��@�9X@���@�C�@���@�M�@��7@�Ĝ@���@��F@�|�@�t�@�\)@�@��H@���@�^5@��#@��h@�p�@��@��D@�I�@��F@���@��@��@���@�+@���@�$�@��h@�G�@��@��@��j@��@�A�@��
@���@��H@��9@}�@u��@l�@d�j@\�/@Tz�@L1@D�@<j@5/@1X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�M�A�K�A�K�A�I�A�C�A�I�A�A�A�?}A�7LA�33A�1'A�$�A�{A�VA��Aڕ�AًDA��/A؏\A�dZA�Q�A�G�A�?}A�9XA� �A�oA���A׺^Aק�Aם�A�z�A��yAվwA�O�A��A�9XA��`A�jA�ƨA�ZA��A��A�A�(�A�^5A�\)A��A���A��TA��PA��HA��`A�Q�A�^5A�E�A��;A�{A���A�l�A���A�A�
=A�-A��A�\)A�;dA�{A�9XA���A�r�A���A�A��uA�?}A� �A���A�z�A���A�  A��A�%A��DA���A�`BA�5?A�A{�Ax~�Aq�Am��Aj �Ag&�Ab��A_�A[�AUC�AQ�AK��AI�AF��AD��AA��A>ȴA<�A9�hA7�hA6ȴA5��A3�A1��A/�A/O�A/�A.bNA-
=A,JA+�mA,bA,^5A,��A-dZA-��A,��A,bNA,r�A+hsA*E�A( �A'oA&�A$ZA#�A"�!A"A!��A!�FA!`BA I�A�hAS�AC�A�hAC�A{AO�Ar�AZAr�A�+AO�AA�A9XA�7A"�AA�uAJA��A�A��AƨA�PA33A�A�PA��A�`AJAJA�wA`BA7LA�A�A�^A|�A�A�/An�A  A�A&�A�A�DAM�A�wAK�AA��A��AZA$�AAp�AK�A7LAv�A�mAK�A��Az�AffAA�A �A��A`BA
��A
Q�A
A�A
1A	A	x�A	;dA	%A��AQ�A1'A=qA(�A�mA��A&�A�A5?A��A;dA
=A�/A��A��A��A?}AVA�A��AĜA�\AbA�FAx�A �A �RA �\A ~�A bNA -A A @���@�|�@��H@��@��7@�`B@�X@��@�Q�@��R@��@��@���@��@�t�@��@���@�V@���@��9@�  @�S�@�n�@�-@�X@��D@�b@�\)@���@�$�@���@웦@�bN@��
@�o@�~�@�@�V@�r�@� �@�@��@�^@�G�@�Ĝ@�r�@��@㕁@�o@�\@�h@�Z@��@߮@�@�J@�`B@�V@��@܃@��@�|�@ڗ�@�$�@��#@�Ĝ@�(�@���@��@�^5@ղ-@�&�@��/@�Z@Ӿw@�S�@җ�@�E�@�@�@�V@Гu@��@�33@���@��@́@�%@̋D@�Z@�b@�\)@��y@ʇ+@�V@�5?@ɉ7@��@�9X@�1@��;@��
@�t�@�@Ƈ+@�M�@��#@ř�@�p�@�7L@ě�@�I�@�1@��
@Õ�@�S�@¸R@�J@���@�Z@��@�ƨ@���@�dZ@�;d@�+@�o@��@�~�@�5?@��@���@��j@�1@�ƨ@���@���@��P@�;d@��@���@���@��@�I�@���@�E�@�5?@��@���@��@�@��h@��@�z�@�r�@�j@��@�ƨ@�C�@�ȴ@��#@��@�bN@�1@���@�|�@�"�@��\@�p�@�9X@��@��@���@�+@��\@�=q@�`B@��@��`@���@�Z@���@�ȴ@�V@�=q@��#@���@��u@�I�@�(�@��@��@�+@��\@�E�@��@�J@��^@�O�@��@��u@��@�9X@���@�C�@���@�M�@��7@�Ĝ@���@��F@�|�@�t�@�\)@�@��H@���@�^5@��#@��h@�p�@��@��D@�I�@��F@���@��@��@���@�+@���@�$�@��h@�G�@��@��@��j@��@�A�G�O�@���@��H@��9@}�@u��@l�@d�j@\�/@Tz�@L1@D�@<j@5/@1X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	gmB	gmB	gmB	gmB	hsB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	gmB	ffB	ffB	jB	�1B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�RB	��B
^5B
�7BG�B��B�B�B��B33BdZBy�B�%B��B�-B�TB�B�B��BB%BDB�B"�B�B�B�B�B�B!�B%�B"�B�B�BB��B�
B�'B��Bw�BN�B�BɺB��B}�BD�B{B
��B
��B
v�B
J�B
)�B
!�B	��B	ĜB	�-B	��B	�B	hsB	S�B	B�B	+B	�B	B�B�BB�B��B�
B�;B�B�B�B�B��B	uB	%�B	�B	VB	JB	{B	!�B	0!B	2-B	:^B	S�B	l�B	}�B	��B	�'B	��B	��B	�fB	�B	�B	�sB	�;B	�#B	��B	ȴB	��B	�}B	�jB	ǮB	��B	��B	ŢB	ƨB	��B	�
B	�`B	�fB	�ZB	�;B	�#B	�fB	�B	��B
	7B
�B
!�B
!�B
�B
�B
VB
bB
oB
oB
�B
#�B
%�B
'�B
(�B
8RB
?}B
<jB
:^B
?}B
?}B
?}B
@�B
<jB
<jB
:^B
:^B
;dB
=qB
=qB
;dB
:^B
8RB
;dB
=qB
<jB
;dB
<jB
;dB
;dB
;dB
:^B
9XB
9XB
9XB
9XB
8RB
7LB
5?B
5?B
49B
49B
49B
5?B
5?B
49B
33B
2-B
2-B
1'B
1'B
1'B
0!B
0!B
0!B
.B
/B
/B
1'B
2-B
33B
2-B
1'B
0!B
/B
/B
.B
/B
/B
/B
-B
,B
+B
,B
.B
/B
-B
,B
(�B
&�B
%�B
%�B
$�B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
#�B
"�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
{B
{B
{B
uB
uB
uB
oB
hB
bB
bB
\B
\B
VB
\B
VB
VB
VB
PB
PB
JB
DB
DB

=B
	7B
	7B
	7B
	7B
1B
+B
%B
+B
1B
+B
B
B
B
B
B
B
B
B
B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
%B
+B
+B
+B
%B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
+B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B
	7B

=B
JB
JB
JB
JB
DB
JB
PB
PB
PB
JB
JB
JB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
VB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
hB
hB
hB
oB
oB
uB
uB
uB
oB
hB
hB
oB
uB
{B
{B
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
$�B
%�B
+B
0!B
49B
8RB
>wB
D�B
I�B
M�B
T�B
ZB
]/B
aHB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	gOB	gOB	gOB	gLB	hUB	gQB	gPB	gRB	gMB	gPB	gOB	gPB	gQB	fHB	fHB	jdB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�1B	ʡB
^B
�BG�B��B�OB�B��B3Bd)By�B��B�eB�B�#B�aB�nB��B�B�BB�B"�BhB{B�B�B�B!�B%�B"�B{BNB�B�B��B��B�gBw�BN�B_BɆB��B}�BDgBHB
бB
��B
v�B
J�B
)�B
!�B	��B	�nB	��B	��B	��B	hDB	S�B	BbB	*�B	eB	�B�mB�B��B��B��B�B�[B�|B�iB�sB��B	DB	%�B	iB	%B	B	JB	!�B	/�B	1�B	:+B	S�B	lYB	}�B	�JB	��B	�RB	ӽB	�-B	�CB	�TB	�:B	�B	��B	��B	�zB	�IB	�AB	�3B	�tB	ѱB	͘B	�hB	�oB	͚B	��B	�'B	�+B	�B	��B	��B	�*B	�\B	��B
�B
zB
!�B
!�B
zB
LB
B
%B
0B
1B
oB
#�B
%�B
'�B
(�B
8B
?>B
<+B
:!B
??B
?@B
?=B
@EB
<*B
<,B
:B
:B
;$B
=4B
=1B
;'B
:B
8B
;$B
=1B
<(B
;'B
<-B
;&B
;%B
;&B
:B
9B
9B
9B
9B
8B
7
B
4�B
5 B
3�B
3�B
3�B
5B
5B
3�B
2�B
1�B
1�B
0�B
0�B
0�B
/�B
/�B
/�B
-�B
.�B
.�B
0�B
1�B
2�B
1�B
0�B
/�B
.�B
.�B
-�B
.�B
.�B
.�B
,�B
+�B
*�B
+�B
-�B
.�B
,�B
+�B
(�B
&�B
%�B
%�B
$�B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
#�B
"�B
!�B
 �B
 �B
�B
�B
vB
jB
jB
gB
eB
eB
\B
\B
XB
RB
MB
NB
GB
HB
@B
:B
<B
9B
6B
8B
:B
8B
4B
4B
5B
-B
(B
!B
"B
B
B
B
B
B
B
B
B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
�B
�B
	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
	B

B
	B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
&B
$B
*B
*B
1B
1B
0B
*B
%B
$B
(B
0B
6B
3B
6B
1B
5B
6B
6B
AB
BB
KB
OB
QB
[B
[B
gB
hB
sB
tB
sB
rB
|B
{B
yB
{B
 �B
 �B
!�B
!�G�O�B
%�B
*�B
/�B
3�B
8B
>2B
DVB
IsB
M�B
T�B
Y�B
\�B
aB
j91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.57 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150703021623    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150703021623  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150703021623  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                