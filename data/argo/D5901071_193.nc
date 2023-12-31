CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:45Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143357  20190522121828  1727_5046_193                   2C  D   APEX                            2143                            040306                          846 @�H?��1   @�����@6�E���c�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`�fDa  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� DlfDl� Dm  Dmy�Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Dr��Dss3Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ffA33A#33AC33Ac33A���A���A���A�ffA���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BHffBP��BX��B`��Bh��Bp��Bx��B�ffB�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C�C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(L�C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ�C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D �3D�D��D3D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D�fD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�3D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=fD=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ�fDK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_�3D`�D`�3Da�Da��Db�Db��Dc�Dc�3Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��DkfDk��Dl3Dl��Dm�Dm�fDn�Dn��Do�Do��DpfDp��Dq�Dq��Dr�Dr��DsfDs� Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�  A�  A�A�1A�1A�
=A�1A�A���A���A��AƸRA�t�A�/A���A���A�;dAę�A�&�A���A�
=A�bNA�"�A�ĜA�bNA��;A��A�  A�n�A��hA��TA���A�(�A��/A�S�A���A�dZA�JA���A�dZA��/A�A��A�A�A�1'A��9A�ffA�E�A�1'A�bA�ffA�%A��PA���A�n�A�7LA�VA��yA���A�l�A��A��A�C�A���A��
A��-A�1'A���A��\A��
A�t�A�dZA�E�A��A�`BA�x�A���A�G�A��FA�|�A�+A�;dA�r�A���A�r�A��#A~��A{x�Az�DAzAy��Ay7LAt�HAr1'Ap�`Ao��AodZAo%Anz�AmO�Ak&�Ah��Ag�Ae��Ac%Aa�TAaXA`�9A_�^A]hsAZQ�AY�AYƨAYVAW�AW�^AWx�AW;dAV�AU33AT^5AR9XAPE�AO33AO33AN��AM�AM�PALVAJbAH��AH^5AHZAH-AG�AG�7AF�DAC+A?�A<��A<�A;�#A;G�A9�-A8��A7��A5�
A4�uA3�#A2��A1\)A0�/A0��A05?A/ƨA/?}A.E�A,9XA*��A)C�A'dZA$��A#�A!��A!oA �RA �A I�Ax�AbNA��A/A�A�+A5?A|�A��AJA\)A��A|�A&�AĜA�A�PA1A
��A	��A	G�A�A(�AdZA�/A�+AM�A"�AA�AJA�A��A�FA��A�PAp�A%A�9A�uA�A �D@���@�Ĝ@��w@��@�G�@�hs@��@��D@�(�@�dZ@���@�t�@��@�-@�&�@�1'@���@�@�S�@���@߶F@�~�@�?}@ܓu@�I�@�1'@�b@���@��
@ە�@�;d@��@���@ڗ�@�-@�7L@�1@���@��@�l�@��@Ұ!@��@ѩ�@�/@мj@Ѓ@�t�@��@�M�@ͩ�@�%@�bN@��@˅@��H@�V@�^5@�-@���@ɲ-@��@���@ǍP@�@���@�bN@��@î@§�@�b@���@���@�=q@�-@��@�{@�{@�{@�J@��@�hs@�?}@�&�@�V@��/@�bN@��@��P@��@���@�{@��^@��7@�p�@�O�@�?}@�V@��D@��m@�K�@�o@��@��@�Q�@���@���@�=q@��#@�`B@��@���@�bN@�I�@���@�"�@��y@�M�@�@�&�@�  @��m@�S�@�v�@�M�@�5?@��@��T@��^@���@�O�@��u@�A�@���@�K�@�ȴ@�p�@���@��m@��F@���@�C�@��@���@��@���@�ff@���@�V@��D@�1@��
@���@���@�S�@�\)@�"�@��@��@��+@�5?@��T@���@�O�@��@�bN@��;@�dZ@�C�@�o@�ȴ@�v�@�E�@�=q@�5?@��T@��T@��T@��#@��#@��T@��T@��#@��^@�G�@��u@�9X@��@���@���@��P@��@�^5@��@��@��#@��^@���@��h@��h@��7@��7@��7@��@�p�@�X@�/@�V@��@�1'@�t�@�ȴ@�V@�5?@��@���@��@�G�@��`@�Ĝ@�z�@��@���@�+@��R@�M�@�5?@�5?@���@�`B@�&�@��@��@�V@�V@��@��j@��@��@��@��@�bN@�Q�@���@���@���@��@�t�@�\)@�S�@�K�@�+@���@�v�@�M�@��T@��^@��-@���@�%@�j@�1@��@��@�"�@��y@�ȴ@���@�$�@�@�p�@���@�j@�1'@�b@��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�  A�  A�A�1A�1A�
=A�1A�A���A���A��AƸRA�t�A�/A���A���A�;dAę�A�&�A���A�
=A�bNA�"�A�ĜA�bNA��;A��A�  A�n�A��hA��TA���A�(�A��/A�S�A���A�dZA�JA���A�dZA��/A�A��A�A�A�1'A��9A�ffA�E�A�1'A�bA�ffA�%A��PA���A�n�A�7LA�VA��yA���A�l�A��A��A�C�A���A��
A��-A�1'A���A��\A��
A�t�A�dZA�E�A��A�`BA�x�A���A�G�A��FA�|�A�+A�;dA�r�A���A�r�A��#A~��A{x�Az�DAzAy��Ay7LAt�HAr1'Ap�`Ao��AodZAo%Anz�AmO�Ak&�Ah��Ag�Ae��Ac%Aa�TAaXA`�9A_�^A]hsAZQ�AY�AYƨAYVAW�AW�^AWx�AW;dAV�AU33AT^5AR9XAPE�AO33AO33AN��AM�AM�PALVAJbAH��AH^5AHZAH-AG�AG�7AF�DAC+A?�A<��A<�A;�#A;G�A9�-A8��A7��A5�
A4�uA3�#A2��A1\)A0�/A0��A05?A/ƨA/?}A.E�A,9XA*��A)C�A'dZA$��A#�A!��A!oA �RA �A I�Ax�AbNA��A/A�A�+A5?A|�A��AJA\)A��A|�A&�AĜA�A�PA1A
��A	��A	G�A�A(�AdZA�/A�+AM�A"�AA�AJA�A��A�FA��A�PAp�A%A�9A�uA�A �D@���@�Ĝ@��w@��@�G�@�hs@��@��D@�(�@�dZ@���@�t�@��@�-@�&�@�1'@���@�@�S�@���@߶F@�~�@�?}@ܓu@�I�@�1'@�b@���@��
@ە�@�;d@��@���@ڗ�@�-@�7L@�1@���@��@�l�@��@Ұ!@��@ѩ�@�/@мj@Ѓ@�t�@��@�M�@ͩ�@�%@�bN@��@˅@��H@�V@�^5@�-@���@ɲ-@��@���@ǍP@�@���@�bN@��@î@§�@�b@���@���@�=q@�-@��@�{@�{@�{@�J@��@�hs@�?}@�&�@�V@��/@�bN@��@��P@��@���@�{@��^@��7@�p�@�O�@�?}@�V@��D@��m@�K�@�o@��@��@�Q�@���@���@�=q@��#@�`B@��@���@�bN@�I�@���@�"�@��y@�M�@�@�&�@�  @��m@�S�@�v�@�M�@�5?@��@��T@��^@���@�O�@��u@�A�@���@�K�@�ȴ@�p�@���@��m@��F@���@�C�@��@���@��@���@�ff@���@�V@��D@�1@��
@���@���@�S�@�\)@�"�@��@��@��+@�5?@��T@���@�O�@��@�bN@��;@�dZ@�C�@�o@�ȴ@�v�@�E�@�=q@�5?@��T@��T@��T@��#@��#@��T@��T@��#@��^@�G�@��u@�9X@��@���@���@��P@��@�^5@��@��@��#@��^@���@��h@��h@��7@��7@��7@��@�p�@�X@�/@�V@��@�1'@�t�@�ȴ@�V@�5?@��@���@��@�G�@��`@�Ĝ@�z�@��@���@�+@��R@�M�@�5?@�5?@���@�`B@�&�@��@��@�V@�V@��@��j@��@��@��@��@�bN@�Q�@���@���@���@��@�t�@�\)@�S�@�K�@�+@���@�v�@�M�@��T@��^@��-@���@�%@�j@�1@��@��@�"�@��y@�ȴ@���@�$�@�@�p�@���@�j@�1'@�b@��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBjBk�Bk�BjBk�Bl�Bl�Bl�Bl�Bk�Bk�Bk�BjBiyBhsBgmBffBe`BdZBcTBjBp�B|�B�B�+B�7B�DB�PB�bB��B��B��B��B��B��B��B��B�B�9B�9B�^B�XB�LB�qB�}BB�wB��B��B��B��B��B�PB�Bv�Bm�BiyBgmBdZB`BBE�B)�B�B�BbBB��B�B�ZB�wB��B�Bk�BZBF�B"�B  B
�B
�ZB
��B
��B
ƨB
ÖB
�'B
��B
�+B
�B
x�B
VB
E�B
?}B
;dB
7LB
0!B
\B
  B	��B	�B	�B	�B	�yB	�TB	�
B	ǮB	�wB	�RB	�B	��B	��B	��B	�bB	�B	s�B	q�B	o�B	k�B	ffB	dZB	bNB	^5B	XB	O�B	H�B	7LB	-B	)�B	)�B	'�B	"�B	 �B	 �B	�B	bB	VB	VB	VB	PB	JB	%B��B�B�TB�TB�NB�;B�B�
B��B��BȴBŢBÖB�}B�wB�jB�dB�XB�XB�-B�B�B��B��B��B��B��B��B��B��B�uB�oB�hB�hB�hB�bB�bB�VB�JB�JB�=B�+B� B}�B~�B}�Bz�Bu�Bu�Bu�Bu�Bt�Bs�Bs�Br�Bq�Bq�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bn�Bn�Bm�Bm�Bl�Bk�BgmBdZBcTBbNB`BB]/BZB\)B[#B[#BZBYBXBYBW
BW
BVBT�BS�BR�BVBXBXBXBZB[#B[#B\)B\)B\)B[#B\)B\)B\)B\)B]/B]/B^5B]/B]/B^5B^5B^5B^5B_;B`BBbNBe`BffBiyBjBiyBgmBgmBhsBiyBiyBjBm�Bq�Bu�Bu�Bx�B� B�B�7B�PB�bB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�'B�3B�9B�FB�XB�dB�jB�qB�wB�}B�wB�}BBŢBȴBɺBȴB��B��B�B�B�/B�;B�NB�TB�ZB�mB�mB�B�B�B��B��B	  B	1B		7B	PB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	)�B	,B	/B	8RB	>wB	B�B	C�B	C�B	E�B	F�B	H�B	H�B	J�B	K�B	N�B	R�B	VB	\)B	_;B	_;B	aHB	dZB	dZB	e`B	gmB	gmB	iyB	l�B	q�B	r�B	t�B	}�B	�B	�+B	�DB	�JB	�PB	�VB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�'B	�-B	�9B	�9B	�LB	�XB	�jB	�jB	�qB	�}B	�}B	��B	B	��B	B	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�BB	�NB	�NB	�NB	�TB	�ZB	�ZB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BjBk�Bk�BjBk�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bk�BjBiyBhsBffBhsBffBe`Bl�Bs�B� B�%B�7B�DB�VB�hB��B��B��B��B��B�B��B�B�B�B�?B�RB�}B�dB�^BBƨB��B��B��B��B��B��B��B��B�7Bz�Bn�BjBgmBdZBiyBN�B0!B�B�B�BB��B�B�BɺB��B�=Bq�BaHBS�B2-BB
��B
�B
�B
��B
ǮB
ƨB
�FB
��B
�1B
�%B
�B
\)B
G�B
@�B
<jB
8RB
9XB
�B
B
  B	�B	�B	�B	�B	�yB	�;B	��B	ÖB	��B	�B	��B	��B	��B	��B	�=B	t�B	r�B	q�B	n�B	gmB	e`B	cTB	aHB	[#B	R�B	N�B	<jB	0!B	)�B	,B	)�B	#�B	$�B	'�B	�B	hB	VB	VB	\B	VB	VB	JB��B�B�`B�ZB�TB�TB�)B�B�
B��B��BȴBǮB��B�}B�qB�jB�dB�jB�RB�-B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�oB�hB�hB�bB�\B�VB�JB�=B�\B�B� B~�B~�B�By�By�Bv�Bv�Bu�Bv�Bt�Br�Br�Bs�Br�Bp�Bo�Bo�Bn�Bn�Bn�Bn�Bn�Bn�Bl�Bl�Bk�BhsBe`BcTBcTBcTB_;B]/B\)B\)B[#B\)B[#BZBXBYBXBW
BVBXBZBZBZBZB[#B\)B[#B\)B\)B\)B\)B]/B\)B\)B]/B^5B_;BaHBaHB`BB_;B_;B_;B_;B`BBaHBbNBffBhsBiyBk�BjBhsBhsBhsBjBjBk�Bm�Bq�Bv�Bu�Bx�B� B�%B�DB�\B�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�-B�9B�?B�LB�^B�jB�jB�qB�wB�}B�wB��BÖBƨBȴB��B��B��B��B�
B�B�5B�BB�TB�ZB�`B�mB�sB�B�B��B��B��B	B	1B	
=B	VB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	+B	-B	1'B	9XB	?}B	B�B	C�B	C�B	E�B	F�B	H�B	H�B	J�B	L�B	O�B	S�B	W
B	\)B	_;B	_;B	aHB	dZB	dZB	e`B	gmB	hsB	jB	l�B	q�B	s�B	u�B	~�B	�B	�1B	�DB	�JB	�VB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�!B	�!B	�!B	�'B	�'B	�'B	�3B	�9B	�?B	�RB	�^B	�jB	�jB	�qB	�}B	�}B	��B	B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�BB	�NB	�NB	�NB	�TB	�ZB	�ZB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447422012010314474220120103144742  AO  ARGQ                                                                        20111130143357  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143357  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144742  IP                  G�O�G�O�G�O�                