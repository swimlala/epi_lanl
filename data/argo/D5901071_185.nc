CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:43Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143320  20190522121827  1727_5046_185                   2C  D   APEX                            2143                            040306                          846 @�Ʃ&O�1   @��Q�o�@5���O�;�c�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D:��D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�ffA33A#33AC33Ac33A���A�ffA���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`ffBhffBp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBܙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33CL�C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<L�C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CTL�CVL�CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�fD�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:�fD;fD;��D<3D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR3DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh3Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��#Aʺ^Aʧ�Aʏ\A�v�A�VA�E�A�9XA�/A�+A�"�A��A�bA�VA�
=A�A���A���A��A��A��;A���A���A�ĜAɾwAɶFAɡ�Aɏ\AɅA�r�A�C�A�7LA�"�A��A�
=A���A���A��A��A��A��A��yA��HA���A�ȴAȏ\A�dZA�-A�?}A��!A���A��#A��+A���A�=qA��A��A��yA��/A���A�ȴA�z�A��uA���A���A���A�1'A���A�&�A���A��A�;dA��FA���A���A��yA�-A���A�?}A���A�v�A�ȴA���A�Q�A��A���A���A��A�bNA�S�A��hA�?}A�r�A�9XA��A�C�A�5?A��A� �A��FA���A���A�;dA�Q�A�?}A�S�A�A�VA�ȴA� �A���A��A��/A��DA�C�A��A��^A}�FA|I�A{�Ax�HAul�Ap�Al��Ai
=Afn�Ae�Ad��Aa�hA`9XA_�A^ffA]�7A[�TAY`BAV�9ASG�AR�AQ�AQ��AQoAPQ�ANJAK�hAIAHQ�AG��AF��AD�uAAG�A>��A=��A<��A;%A9�mA9��A9dZA9"�A8��A8JA7��A5S�A3�A/��A-XA,9XA,1A+��A+"�A*v�A)XA&�uA$1A#x�A#&�A!��A ��A��A�A;dA(�Ap�A��A5?A��A��A;dA�7A�hA33A&�AffAJA~�A��A�A^5A-A��A��Al�A
5?A	�wA��A�AO�A^5A9XA=qA��AXAȴAp�AbA�mA�FA �u@��\@���@�9X@���@��@�l�@���@�@�V@�  @�@���@�@�v�@��@��@��@�K�@�n�@���@�=q@��H@�I�@�E�@��@�9@�C�@�V@��@�%@ܓu@���@�M�@ج@ם�@�;d@և+@���@�Z@ҏ\@љ�@ѡ�@���@��@��T@с@�I�@��H@�%@�ƨ@�|�@��@�o@��H@�x�@�r�@�  @Ǿw@�;d@��@ƸR@Ƈ+@�M�@��@�ƨ@�@�=q@���@�X@���@��D@�j@���@��w@��H@�$�@���@�X@��@�@��@�hs@��
@�^5@��#@�O�@��9@� �@�ƨ@�
=@��+@�|�@�z�@�z�@�Z@�Q�@��F@��@��@�%@��u@��@��@��@��D@��u@�z�@�9X@���@�l�@�ȴ@�5?@���@��@��7@���@�`B@���@�I�@�ƨ@�l�@�{@�bN@�bN@�j@�r�@��m@��y@��@�x�@��h@���@��@�~�@��!@�S�@��
@�+@�@�/@��9@�b@��
@�l�@���@�v�@�5?@�V@��T@��@�?}@��@���@�1'@��w@�t�@�K�@�o@�v�@�E�@�=q@��T@�{@��!@���@���@�^5@�$�@�J@���@��^@���@�V@��@�I�@�  @���@���@�|�@�t�@�|�@��@�S�@��y@���@���@�E�@��@���@�7L@�1@��F@��@��@��@���@�~�@���@�%@��9@�Z@� �@��@�dZ@�ȴ@��!@���@���@�ff@��#@��@�(�@��w@��@�\)@�+@�o@��y@��@��@��H@��@���@�n�@��R@��H@��@��+@�ff@�E�@�5?@��@���@��-@�/@��`@���@��9@��@�z�@��
@�S�@�@��H@��H@���@�~�@�ff@�=q@�{@�@���@��#@�X@��@�&�@��@�%@��`@���@��@���@�S�@�;d@�
=@��y@�~�@�E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��#Aʺ^Aʧ�Aʏ\A�v�A�VA�E�A�9XA�/A�+A�"�A��A�bA�VA�
=A�A���A���A��A��A��;A���A���A�ĜAɾwAɶFAɡ�Aɏ\AɅA�r�A�C�A�7LA�"�A��A�
=A���A���A��A��A��A��A��yA��HA���A�ȴAȏ\A�dZA�-A�?}A��!A���A��#A��+A���A�=qA��A��A��yA��/A���A�ȴA�z�A��uA���A���A���A�1'A���A�&�A���A��A�;dA��FA���A���A��yA�-A���A�?}A���A�v�A�ȴA���A�Q�A��A���A���A��A�bNA�S�A��hA�?}A�r�A�9XA��A�C�A�5?A��A� �A��FA���A���A�;dA�Q�A�?}A�S�A�A�VA�ȴA� �A���A��A��/A��DA�C�A��A��^A}�FA|I�A{�Ax�HAul�Ap�Al��Ai
=Afn�Ae�Ad��Aa�hA`9XA_�A^ffA]�7A[�TAY`BAV�9ASG�AR�AQ�AQ��AQoAPQ�ANJAK�hAIAHQ�AG��AF��AD�uAAG�A>��A=��A<��A;%A9�mA9��A9dZA9"�A8��A8JA7��A5S�A3�A/��A-XA,9XA,1A+��A+"�A*v�A)XA&�uA$1A#x�A#&�A!��A ��A��A�A;dA(�Ap�A��A5?A��A��A;dA�7A�hA33A&�AffAJA~�A��A�A^5A-A��A��Al�A
5?A	�wA��A�AO�A^5A9XA=qA��AXAȴAp�AbA�mA�FA �u@��\@���@�9X@���@��@�l�@���@�@�V@�  @�@���@�@�v�@��@��@��@�K�@�n�@���@�=q@��H@�I�@�E�@��@�9@�C�@�V@��@�%@ܓu@���@�M�@ج@ם�@�;d@և+@���@�Z@ҏ\@љ�@ѡ�@���@��@��T@с@�I�@��H@�%@�ƨ@�|�@��@�o@��H@�x�@�r�@�  @Ǿw@�;d@��@ƸR@Ƈ+@�M�@��@�ƨ@�@�=q@���@�X@���@��D@�j@���@��w@��H@�$�@���@�X@��@�@��@�hs@��
@�^5@��#@�O�@��9@� �@�ƨ@�
=@��+@�|�@�z�@�z�@�Z@�Q�@��F@��@��@�%@��u@��@��@��@��D@��u@�z�@�9X@���@�l�@�ȴ@�5?@���@��@��7@���@�`B@���@�I�@�ƨ@�l�@�{@�bN@�bN@�j@�r�@��m@��y@��@�x�@��h@���@��@�~�@��!@�S�@��
@�+@�@�/@��9@�b@��
@�l�@���@�v�@�5?@�V@��T@��@�?}@��@���@�1'@��w@�t�@�K�@�o@�v�@�E�@�=q@��T@�{@��!@���@���@�^5@�$�@�J@���@��^@���@�V@��@�I�@�  @���@���@�|�@�t�@�|�@��@�S�@��y@���@���@�E�@��@���@�7L@�1@��F@��@��@��@���@�~�@���@�%@��9@�Z@� �@��@�dZ@�ȴ@��!@���@���@�ff@��#@��@�(�@��w@��@�\)@�+@�o@��y@��@��@��H@��@���@�n�@��R@��H@��@��+@�ff@�E�@�5?@��@���@��-@�/@��`@���@��9@��@�z�@��
@�S�@�@��H@��H@���@�~�@�ff@�=q@�{@�@���@��#@�X@��@�&�@��@�%@��`@���@��@���@�S�@�;d@�
=@��y@�~�@�E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŢB�FB�3B�B�B�B��B��B��B��B��B��B��B��B��B��B�!B�9B�FB�RB�FB�jB�RB�!B��B��B��B�PB�1B� By�BZBB�B:^B,B�BhBbBPB
=BB��B�B�B�sB�;B�#B��B��B��BƨB�^B��B�hB�1Bp�BbNBZBI�B$�BB
�BB
ĜB
�LB
��B
��B
�oB
�JB
� B
[#B
A�B
6FB
-B
�B	��B	�/B	ǮB	�9B	��B	��B	��B	�=B	�B	~�B	x�B	s�B	jB	\)B	L�B	?}B	;dB	:^B	8RB	49B	/B	%�B	�B	�B	hB	VB	1B��B�B�B�`B�HB�/B�B�B�B�
B�B�B��BɺBÖB�^B�9B�-B�'B�B�B��B��B��B��B��B��B��B�uB�uB�hB�bB�VB�PB�JB�JB�DB�7B�%B�B�B|�B{�B{�Bx�Bw�Bv�Bw�Bz�B{�B|�Bw�Bt�Br�Bs�Br�Bq�Bp�Br�Br�Br�Bp�Bn�Bk�BffBhsBgmBe`BgmBdZBbNBcTBdZBhsBl�Bo�Bo�Bq�Bq�Bs�Bq�Bm�Bk�Bk�Bn�Bp�Bt�Bw�Bz�B|�B�B~�B�B�B�B�+B�DB�JB�oB�{B�{B�bB�PB�bB��B��B��B��B�uB�oB��B��B��B��B��B��B��B��B��B�B�'B�^B�qBƨB��B��B��B��B��B��B��B��B��B�B�#B�BB�NB�NB�NB�`B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	oB	�B	�B	 �B	"�B	%�B	'�B	'�B	+B	-B	/B	/B	/B	0!B	0!B	0!B	0!B	0!B	/B	-B	/B	1'B	33B	49B	8RB	8RB	<jB	=qB	?}B	>wB	;dB	<jB	>wB	?}B	A�B	E�B	D�B	C�B	C�B	H�B	H�B	J�B	O�B	Q�B	W
B	\)B	`BB	aHB	cTB	e`B	iyB	l�B	k�B	iyB	gmB	k�B	o�B	u�B	v�B	v�B	w�B	w�B	x�B	{�B	|�B	}�B	}�B	�B	�B	�B	�B	�DB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�3B	�9B	�9B	�?B	�LB	�LB	�RB	�XB	�qB	�wB	�wB	�qB	�qB	�wB	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	ǮB	ǮB	ǮB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�fB	�fB	�fB	�fB	�fB	�ZB	�ZB	�`B	�`B	�fB	�fB	�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB�FB�B�B�B�B��B��B��B��B��B��B��B�B�B�3B�?B�RB�dB��BɺBƨB�9B�B��B��B�oB�DB�B�B`BBC�B=qB0!B�BoBhBVBJB
=B��B��B�B�B�BB�5B��B��B��B��B��B��B��B�bBt�BgmB]/BP�B.B
=B
�fB
��B
�wB
��B
��B
�uB
�oB
�7B
cTB
D�B
9XB
33B
&�B
DB	�sB	��B	�jB	��B	��B	��B	�VB	�B	�B	{�B	x�B	r�B	e`B	XB	C�B	<jB	;dB	:^B	7LB	7LB	.B	"�B	�B	oB	hB	PB	%B��B�B�mB�`B�BB�#B�B�B�B�B�
B��B��B��BB�LB�3B�3B�'B�B�B�B��B��B��B��B��B��B�{B�uB�{B�hB�bB�PB�PB�JB�DB�JB�1B�B�B}�B}�B}�Bz�B}�B{�B{�B}�B� B}�Bx�Bt�Bv�Bu�Bs�Bs�Br�Br�Bs�Br�Bp�Bo�BjBiyBhsBhsBk�Be`BdZBe`BgmBjBm�Bp�Bq�Bs�Bs�Bt�Bs�Bq�Bo�Bm�Bq�Br�Bu�Bx�Bz�B{�B�B�B�B�%B�+B�7B�JB�VB�uB��B��B�oB�VB�hB��B��B��B��B�{B�oB��B��B��B��B��B��B��B��B��B�B�'B�^B�}BȴB��B��B��B��B��B��B��B��B�
B�B�)B�HB�TB�TB�NB�`B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	hB	�B	�B	 �B	#�B	&�B	(�B	)�B	,B	-B	/B	/B	/B	0!B	0!B	0!B	0!B	1'B	0!B	.B	0!B	1'B	33B	49B	8RB	9XB	=qB	>wB	@�B	@�B	>wB	<jB	>wB	?}B	B�B	G�B	E�B	D�B	C�B	J�B	K�B	K�B	O�B	P�B	VB	]/B	bNB	cTB	dZB	ffB	iyB	m�B	l�B	jB	gmB	k�B	p�B	v�B	v�B	w�B	x�B	x�B	x�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�=B	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�?B	�?B	�LB	�LB	�XB	�^B	�qB	�wB	�wB	�qB	�wB	�}B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	ǮB	ǮB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�NB	�TB	�ZB	�fB	�fB	�fB	�fB	�mB	�`B	�ZB	�`B	�`B	�mB	�fB	�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<49X<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447392012010314473920120103144739  AO  ARGQ                                                                        20111130143320  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143320  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144739  IP                  G�O�G�O�G�O�                