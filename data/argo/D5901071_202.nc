CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:48Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143438  20190522121828  1727_5046_202                   2C  D   APEX                            2143                            040306                          846 @��� 1   @����?�@7[�l�C��c�     1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>fD>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dy9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33A33A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�ffB ��B��B��BffB ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"�C$�C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�3D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5�3D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<�3D=�D=��D>3D>��D?�D?��D@�D@��DA�DA�3DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�fDyFf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA��#A��yA��yA��yA��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�%A�%A�%A�1A�
=A�1A�
=A�JA�VA�1A�A�%A�1A�%A�A�1A�A�
=A�VA���A�VA��A��DA���A� �A�M�A�{A���A��HA��A��!A�x�A��mA�+A��!A���A��FA�dZA�&�A���A��
A��`A�ĜA��A�t�A�"�A�5?A�+A��A�Q�A�  A�"�A�|�A���A���A��A�t�A�M�A�=qA��HA���A���A�JA��RA���A��HA��A�$�A��yA��hA�(�A�"�A��#A�VA�  A�v�A��yA�^5A��A�  A��A~9XA}
=Az��Aw��AuO�As�mApbNAm��Ak�mAi��Af�`Ad�Ad�AcdZAa�7A_��A^�/A^-A]dZA\A�AZjAX�AW�AW`BAV��AU\)ATVAS�^AR�AQ�7AP1'APM�AP(�AK��AHM�AG&�ADbNAB��A@ffA?+A>A�A;��A:�RA8 �A5K�A3�A1�A/��A.ȴA.{A-��A,M�A+p�A*�/A)ƨA(n�A'��A'�hA&^5A%O�A$�A$(�A#�^A"�A!ƨA!dZA!�A �`A n�A;dA33A��A^5A1'A1A�mAp�A�A�TA
=AȴAA�A�mAA/A�A��A��A�A=qAp�A%AQ�A��A
~�A
A	�FA	�A	
=A1'A�A��A  A?}A�/A��A+@���@�X@���@�r�@�M�@�O�@��;@��@�7L@�1@�
=@�\@�=q@�7L@웦@�1'@���@���@�|�@�33@��@�=q@�hs@��@��H@�hs@��m@�o@�v�@��T@�/@��@�1'@�dZ@��@ݺ^@�O�@��@ܴ9@�bN@�A�@�9X@� �@��
@�;d@��@��y@ڇ+@�hs@׾w@�=q@�?}@���@�1@�t�@�
=@�
=@��H@�V@�J@ѩ�@���@���@Гu@�Z@��@ˮ@�^5@ɩ�@Ȭ@��@ź^@��/@�dZ@�l�@�ff@�O�@���@��@��;@�^5@�O�@��`@�I�@�o@���@��`@��9@��j@���@��@�Q�@�
=@�v�@��@�^5@���@�hs@���@�9X@���@�$�@�V@�Ĝ@�%@��@��R@�Ĝ@�S�@���@�E�@���@�1@��@���@�I�@��@��h@��-@���@�p�@�%@���@��@�9X@��m@�K�@��!@�ff@�J@�x�@�V@��/@�Ĝ@�9X@���@��H@���@�V@��@���@���@�O�@��@��u@�Q�@��m@�dZ@�\)@�K�@�K�@�+@��H@���@�v�@�{@��@���@�x�@�X@�O�@�7L@�V@��u@� �@��;@��@��R@�5?@�J@�@�hs@�G�@���@��/@���@�Ĝ@��D@�I�@�A�@�  @���@���@�C�@��@�"�@�33@�;d@�
=@��H@�ȴ@��!@�~�@�^5@��@��@�@�x�@�O�@�G�@�7L@�/@���@� �@��m@��
@��w@�l�@�
=@���@��@��#@��-@��@��u@�Z@�9X@��;@���@�\)@�33@�"�@��@��\@�5?@�J@���@��@��#@���@�&�@��@���@�r�@�r�@�(�@��@���@�dZ@��P@���@��@���@�^5@�E�@��@�@��#@��h@��7@��@�G�@���@�9X@� �@��;@��w@��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ȴA��#A��yA��yA��yA��A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�%A�%A�%A�1A�
=A�1A�
=A�JA�VA�1A�A�%A�1A�%A�A�1A�A�
=A�VA���A�VA��A��DA���A� �A�M�A�{A���A��HA��A��!A�x�A��mA�+A��!A���A��FA�dZA�&�A���A��
A��`A�ĜA��A�t�A�"�A�5?A�+A��A�Q�A�  A�"�A�|�A���A���A��A�t�A�M�A�=qA��HA���A���A�JA��RA���A��HA��A�$�A��yA��hA�(�A�"�A��#A�VA�  A�v�A��yA�^5A��A�  A��A~9XA}
=Az��Aw��AuO�As�mApbNAm��Ak�mAi��Af�`Ad�Ad�AcdZAa�7A_��A^�/A^-A]dZA\A�AZjAX�AW�AW`BAV��AU\)ATVAS�^AR�AQ�7AP1'APM�AP(�AK��AHM�AG&�ADbNAB��A@ffA?+A>A�A;��A:�RA8 �A5K�A3�A1�A/��A.ȴA.{A-��A,M�A+p�A*�/A)ƨA(n�A'��A'�hA&^5A%O�A$�A$(�A#�^A"�A!ƨA!dZA!�A �`A n�A;dA33A��A^5A1'A1A�mAp�A�A�TA
=AȴAA�A�mAA/A�A��A��A�A=qAp�A%AQ�A��A
~�A
A	�FA	�A	
=A1'A�A��A  A?}A�/A��A+@���@�X@���@�r�@�M�@�O�@��;@��@�7L@�1@�
=@�\@�=q@�7L@웦@�1'@���@���@�|�@�33@��@�=q@�hs@��@��H@�hs@��m@�o@�v�@��T@�/@��@�1'@�dZ@��@ݺ^@�O�@��@ܴ9@�bN@�A�@�9X@� �@��
@�;d@��@��y@ڇ+@�hs@׾w@�=q@�?}@���@�1@�t�@�
=@�
=@��H@�V@�J@ѩ�@���@���@Гu@�Z@��@ˮ@�^5@ɩ�@Ȭ@��@ź^@��/@�dZ@�l�@�ff@�O�@���@��@��;@�^5@�O�@��`@�I�@�o@���@��`@��9@��j@���@��@�Q�@�
=@�v�@��@�^5@���@�hs@���@�9X@���@�$�@�V@�Ĝ@�%@��@��R@�Ĝ@�S�@���@�E�@���@�1@��@���@�I�@��@��h@��-@���@�p�@�%@���@��@�9X@��m@�K�@��!@�ff@�J@�x�@�V@��/@�Ĝ@�9X@���@��H@���@�V@��@���@���@�O�@��@��u@�Q�@��m@�dZ@�\)@�K�@�K�@�+@��H@���@�v�@�{@��@���@�x�@�X@�O�@�7L@�V@��u@� �@��;@��@��R@�5?@�J@�@�hs@�G�@���@��/@���@�Ĝ@��D@�I�@�A�@�  @���@���@�C�@��@�"�@�33@�;d@�
=@��H@�ȴ@��!@�~�@�^5@��@��@�@�x�@�O�@�G�@�7L@�/@���@� �@��m@��
@��w@�l�@�
=@���@��@��#@��-@��@��u@�Z@�9X@��;@���@�\)@�33@�"�@��@��\@�5?@�J@���@��@��#@���@�&�@��@���@�r�@�r�@�(�@��@���@�dZ@��P@���@��@���@�^5@�E�@��@�@��#@��h@��7@��@�G�@���@�9X@� �@��;@��w@��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B^5B_;B_;B_;B_;B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B\)BS�BM�BE�B,B��B�HB�3B��B�bB�1B�B�B��B�qB��B�5B�B�B��B��B�
B�;B�)B��B�FB�oB@�B%�B{B��B�
B��B�\B|�B{�Bx�Bv�Bo�B[#BH�B6FB �B�BoB%B
��B
��B
�B
�B
�#B
ǮB
�jB
�RB
�'B
�B
��B
��B
�VB
�%B
x�B
dZB
K�B
@�B
1'B
�B
B	��B	�mB	��B	ĜB	�LB	��B	��B	�uB	�PB	�B	{�B	v�B	r�B	k�B	dZB	[#B	S�B	N�B	L�B	H�B	C�B	?}B	=qB	6FB	0!B	.B	9XB	9XB	�B	%B	B��B��B�B�yB�TB�B��BB�jB�XB�LB�'B�B�B�B�!B��B��B��B��B��B��B��B��B��B��B�{B�oB�uB�oB�hB�bB�VB�DB�DB�=B�=B�7B�7B�1B�%B�B�B�B�B�B�B� B}�B|�Bz�By�By�Bw�Bw�Bv�Bt�Bp�Bn�Bn�Bm�Bl�Bk�BjBhsBk�BjBk�BjBjBjBjBjBjBk�Bm�Bn�Bo�Bo�Bp�Bq�Br�Bs�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bu�Bt�Bu�Bv�Bv�Bv�Bx�Bx�By�By�Bz�Bz�Bz�B{�B{�B|�B}�B}�B~�B� B� B� B� B� B�B�B�B�B�B�%B�7B�DB�DB�JB�PB�VB�PB�PB�VB�VB�\B�hB�hB�bB�\B�oB��B��B��B��B��B��B�B�'B�?B�FB�dB�}B�}B�}B��BBÖBÖBĜBŢBǮB��B��B��B��B��B��B��B��B�#B�/B�`B�`B�`B�fB�ZB�HB�TB�fB�fB�ZB�NB�TB�NB�BB�HB�ZB�yB�B��B	%B	PB	bB	bB	hB	{B	�B	�B	�B	�B	�B	#�B	%�B	'�B	,B	/B	1'B	1'B	5?B	;dB	@�B	B�B	E�B	F�B	G�B	G�B	H�B	J�B	K�B	L�B	N�B	Q�B	R�B	R�B	S�B	VB	ZB	]/B	^5B	_;B	_;B	_;B	bNB	cTB	cTB	cTB	dZB	hsB	l�B	m�B	n�B	o�B	p�B	q�B	s�B	t�B	t�B	u�B	w�B	x�B	y�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�+B	�7B	�=B	�=B	�DB	�JB	�PB	�PB	�PB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�XB	�qB	�wB	�wB	�wB	��B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B^5B_;B_;B_;B_;B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B]/BT�BN�BJ�B5?BB�B�qB��B�{B�=B�1B�B��B�}B��B�;B�)B�B�B��B�
B�BB�5B�B�qB��BF�B)�B�B��B�/B��B�{B|�B|�Bx�Bw�Bu�BcTBO�B=qB$�B�B�BDB  B
��B
��B
�B
�HB
��B
�qB
�^B
�3B
�B
��B
��B
�bB
�=B
}�B
m�B
N�B
F�B
9XB
�B
	7B
B	�B	�B	��B	��B	�B	��B	��B	�uB	�7B	~�B	x�B	u�B	o�B	jB	bNB	VB	O�B	N�B	M�B	F�B	A�B	@�B	:^B	49B	.B	:^B	A�B	�B	1B	+B��B��B��B�B�sB�)B�
BȴB�}B��B�dB�3B�'B�B�B�3B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�oB�oB�hB�PB�DB�DB�=B�=B�=B�VB�+B�%B�B�B�B�B�B�B�B~�B{�B{�By�Bx�Bx�Bx�Bv�Bo�Bo�Bn�Bm�Bn�Bn�Bm�Bn�Bl�Bl�Bm�Bm�Bn�Bn�Bm�Bo�Bn�Bo�Bp�Bq�Bq�Br�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bu�Bv�Bv�Bx�Bw�Bx�Bx�By�By�Bz�Bz�B{�B{�B{�B|�B}�B}�B}�B~�B� B� B� B� B�B�B�B�B�B�B�+B�7B�DB�JB�PB�PB�VB�VB�PB�VB�\B�\B�bB�hB�hB�hB�oB��B��B��B��B��B��B��B�B�'B�LB�RB�jB��B��B��BBÖBĜBŢBƨBǮBǮB��B��B��B��B�
B��B��B��B�#B�/B�fB�fB�fB�sB�fB�HB�TB�fB�yB�mB�ZB�ZB�TB�NB�NB�ZB�yB�B��B	B	PB	bB	bB	oB	{B	�B	�B	�B	�B	 �B	$�B	&�B	(�B	-B	/B	1'B	2-B	6FB	<jB	@�B	C�B	E�B	F�B	G�B	H�B	I�B	K�B	K�B	M�B	N�B	Q�B	R�B	R�B	S�B	W
B	ZB	^5B	_;B	_;B	_;B	_;B	bNB	cTB	cTB	cTB	e`B	iyB	l�B	m�B	o�B	p�B	p�B	q�B	s�B	t�B	t�B	u�B	w�B	x�B	y�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�+B	�7B	�=B	�=B	�DB	�JB	�PB	�PB	�PB	�\B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�9B	�?B	�XB	�qB	�wB	�}B	�wB	��B	ÖB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447452012010314474520120103144745  AO  ARGQ                                                                        20111130143438  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143438  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144745  IP                  G�O�G�O�G�O�                