CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:44Z UW 3.1 conversion   
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143334  20190522121827  1727_5046_188                   2C  D   APEX                            2143                            040306                          846 @���S�1   @��5��@5�������c���"��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�ffA33A#33AC33Ac33A���A���A���A���A���Aљ�A�ffA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C�C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.�fD/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D53D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ3DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db3Db��Dc�Dc�3Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�fDr�Dr��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴA���A���A���A���A���A���A���A���A���A���A��#A��`A��mA��A��A���A�A��A��HA��;A��A��A���A�  A�%A�VA��A��A��A��mAƣ�A�=qA��A��/AžwA�  A�E�A��AÁA��`A\A�x�A�n�A�
=A���A�^5A��A�%A�A��uA��hA�oA�|�A��A�?}A���A�hsA��/A��A���A��hA��;A�VA�VA�ȴA�
=A�jA��9A��A�t�A�hsA���A��7A�S�A���A�A�5?A�hsA��!A�C�A��7A�A�A��`A�bNA��A��\A�ffA��
A��A�v�A���A��A���A�E�A���A���A��jA���A��
A�VA��^A��A~�yA}�mA}XA{AyAw�-Au��As�FAq�7Aq%Ao�;An��An�!Al��Ai�Ag�AghsAgO�Ad  Aa%A_|�A]�A^A[�-AZr�AZQ�AYp�AXA�AV�HAV�uAU�;AS��AP�9AMK�ALM�AK�
AJ�AIO�AHVAF��ADbNA@�`A>A;�-A9�A7��A6Q�A5ƨA5\)A4��A4I�A3|�A2�9A1�;A0��A/x�A/�A.n�A-�A-dZA-K�A,��A+��A+�A+A*�9A)�7A&�!A$^5A#�A"�HA"bNA"=qA ��A�A�FAĜAv�A^5A��AC�A�#A��AjA�^Ap�A7LAZA�-AE�A=qA�#A��A\)A&�A�A�AffAp�A��AM�A�A��A
�A
  A�jA��Av�A�-A%Av�A�A �\@���@�n�@��9@�(�@��@���@���@�bN@�@���@�-@��@�j@�  @�
=@�@�\)@���@�Q�@�(�@���@��
@���@��;@��
@�\)@�@�w@��@�7@�O�@��@�1@�"�@��@�V@�O�@�bN@��@ە�@�o@�n�@�J@���@�`B@���@�A�@׮@ׅ@�@ղ-@��@���@�j@�1'@���@ӝ�@�o@�$�@��@��@�(�@�K�@��@Ͳ-@��m@��@ɲ-@�`B@�/@�%@ȴ9@�9X@�;d@�~�@�=q@�-@�G�@�r�@��;@���@���@�E�@�V@��m@�;d@��\@�$�@���@���@��@��
@�@�ff@�hs@��@��F@�S�@��y@�$�@�I�@��@��@�9X@�dZ@�M�@���@��@�&�@��F@�33@��y@���@�M�@���@�/@���@��@�j@�Q�@�A�@�  @�|�@��@��@���@��+@�M�@�5?@�{@��T@�p�@��/@���@�b@��@��@�dZ@�+@���@��R@�n�@�@���@�`B@���@�(�@��
@�ƨ@��F@�\)@��y@��!@�@�x�@��@��/@��D@�I�@� �@��@��
@�ƨ@���@��P@�t�@�\)@�
=@���@��+@�^5@�^5@�M�@�-@��@���@���@��@�G�@��@��`@��`@���@�Ĝ@���@�Z@�b@��w@��@���@���@�S�@��@��!@��\@��+@�M�@�{@��T@���@�&�@���@�Q�@�A�@� �@��
@���@�|�@�33@��@���@��@�J@��T@�&�@�
=@��@�ƨ@��@���@�ƨ@��@�\)@�C�@�o@�ȴ@���@���@��+@�^5@���@��^@�x�@�`B@�`B@�`B@�X@��`@�1'@� �@� �@� �@�(�@�A�@�I�@�9X@��@�@���@��\@��\@�ff@��7@�1'@��@�o@���@�-@�{@�@��T@���@�x�@���@��@���@���@��D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ȴA���A���A���A���A���A���A���A���A���A���A��#A��`A��mA��A��A���A�A��A��HA��;A��A��A���A�  A�%A�VA��A��A��A��mAƣ�A�=qA��A��/AžwA�  A�E�A��AÁA��`A\A�x�A�n�A�
=A���A�^5A��A�%A�A��uA��hA�oA�|�A��A�?}A���A�hsA��/A��A���A��hA��;A�VA�VA�ȴA�
=A�jA��9A��A�t�A�hsA���A��7A�S�A���A�A�5?A�hsA��!A�C�A��7A�A�A��`A�bNA��A��\A�ffA��
A��A�v�A���A��A���A�E�A���A���A��jA���A��
A�VA��^A��A~�yA}�mA}XA{AyAw�-Au��As�FAq�7Aq%Ao�;An��An�!Al��Ai�Ag�AghsAgO�Ad  Aa%A_|�A]�A^A[�-AZr�AZQ�AYp�AXA�AV�HAV�uAU�;AS��AP�9AMK�ALM�AK�
AJ�AIO�AHVAF��ADbNA@�`A>A;�-A9�A7��A6Q�A5ƨA5\)A4��A4I�A3|�A2�9A1�;A0��A/x�A/�A.n�A-�A-dZA-K�A,��A+��A+�A+A*�9A)�7A&�!A$^5A#�A"�HA"bNA"=qA ��A�A�FAĜAv�A^5A��AC�A�#A��AjA�^Ap�A7LAZA�-AE�A=qA�#A��A\)A&�A�A�AffAp�A��AM�A�A��A
�A
  A�jA��Av�A�-A%Av�A�A �\@���@�n�@��9@�(�@��@���@���@�bN@�@���@�-@��@�j@�  @�
=@�@�\)@���@�Q�@�(�@���@��
@���@��;@��
@�\)@�@�w@��@�7@�O�@��@�1@�"�@��@�V@�O�@�bN@��@ە�@�o@�n�@�J@���@�`B@���@�A�@׮@ׅ@�@ղ-@��@���@�j@�1'@���@ӝ�@�o@�$�@��@��@�(�@�K�@��@Ͳ-@��m@��@ɲ-@�`B@�/@�%@ȴ9@�9X@�;d@�~�@�=q@�-@�G�@�r�@��;@���@���@�E�@�V@��m@�;d@��\@�$�@���@���@��@��
@�@�ff@�hs@��@��F@�S�@��y@�$�@�I�@��@��@�9X@�dZ@�M�@���@��@�&�@��F@�33@��y@���@�M�@���@�/@���@��@�j@�Q�@�A�@�  @�|�@��@��@���@��+@�M�@�5?@�{@��T@�p�@��/@���@�b@��@��@�dZ@�+@���@��R@�n�@�@���@�`B@���@�(�@��
@�ƨ@��F@�\)@��y@��!@�@�x�@��@��/@��D@�I�@� �@��@��
@�ƨ@���@��P@�t�@�\)@�
=@���@��+@�^5@�^5@�M�@�-@��@���@���@��@�G�@��@��`@��`@���@�Ĝ@���@�Z@�b@��w@��@���@���@�S�@��@��!@��\@��+@�M�@�{@��T@���@�&�@���@�Q�@�A�@� �@��
@���@�|�@�33@��@���@��@�J@��T@�&�@�
=@��@�ƨ@��@���@�ƨ@��@�\)@�C�@�o@�ȴ@���@���@��+@�^5@���@��^@�x�@�`B@�`B@�`B@�X@��`@�1'@� �@� �@� �@�(�@�A�@�I�@�9X@��@�@���@��\@��\@�ff@��7@�1'@��@�o@���@�-@�{@�@��T@���@�x�@���@��@���@���@��D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B@�B@�BA�B@�BA�BA�BA�BB�BB�BB�BC�BE�BF�BG�BH�BL�BR�BbNB�B�\B��B��B��B��B�B�9B�XB�wB��B�B�ZB�mB�yB�sB�mB�sB�ZB�TB�ZB�sB�B�B�B�B�sB�sB�fB�ZB�TB�;B�BB�BB�fB�sB�sB�mB�NB��B��B�B�
B��B��BǮB��B�XB�B��B��B�\Bk�BG�B49B"�B1B�B��BĜB�XB�-B��B��B��B�oB�=Bu�BcTBYBJ�B?}B-B
��B
�B
��B
ÖB
�?B
��B
�DB
y�B
q�B
gmB
W
B
O�B
D�B
@�B
:^B
33B
"�B
�B
%B	��B	�B	�B	�`B	�HB	�
B	ƨB	�9B	�-B	�B	��B	�%B	z�B	l�B	n�B	]/B	R�B	P�B	J�B	D�B	;dB	9XB	33B	&�B	oB	B��B��B�B�B�B��B�`B�
B��BɺBƨB��B�LB�RB�dB�jB�RB�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�bB�PB�DB�=B�1B�%B�B�B�B�%B�%B�%B�B�B�B�%B�B�B�B�+B�B�B�B� B~�B~�B}�B{�Bz�By�By�By�By�Bw�Bu�Bu�Bv�Bv�Bu�By�Bt�Br�Bm�BffBe`BiyBiyBl�Bl�Bk�Bl�Bn�Bo�Bp�Bq�Bq�Br�Br�Bs�Bs�Bx�Bw�B|�B~�B� B�B�B�B�B�%B�B�B�B�B�B�+B�JB�VB�bB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�-B�9B�9B�9B�?B�LB�dB�jB�jB�qB�qB�wB��BĜBǮBɺBȴB��B��B�B�B�#B�#B�5B�ZB�yB�B�B�B��B	  B	B	DB	PB	bB	uB	uB	oB	bB	PB	JB	hB	bB	bB	\B	PB	oB	{B	�B	�B	�B	�B	�B	 �B	 �B	#�B	$�B	&�B	&�B	&�B	&�B	'�B	+B	-B	.B	.B	/B	0!B	1'B	1'B	33B	6FB	:^B	;dB	@�B	C�B	D�B	E�B	G�B	H�B	J�B	K�B	M�B	O�B	P�B	R�B	XB	ZB	[#B	[#B	]/B	aHB	bNB	e`B	hsB	k�B	l�B	n�B	p�B	q�B	s�B	t�B	t�B	u�B	v�B	v�B	w�B	z�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�RB	�jB	�jB	�qB	��B	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	ɺB	ȴB	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�`B	�mB	�mB	�mB	�fB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�yB	�yB	�B	�B	�B	�yB	�sB	�`B	�TB	�HB	�BB	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B@�B@�B@�BA�B@�BA�BA�BA�BB�BB�BB�BC�BE�BF�BG�BH�BL�BR�BbNB�B�\B��B��B��B��B�B�9B�XB�wB��B�#B�fB�sB�yB�yB�B�B�fB�`B�mB�B�B�B�B�B�B�B�sB�`B�TB�mB�sB�B�B�B�B�B�sB��B��B�)B�B�B��BɺBÖB�}B�-B��B��B��Bv�BM�B:^B)�BoB��B�
BȴB�dB�FB��B��B��B��B�hB{�BffB^5BM�BE�BB�BB
�;B
��B
ȴB
�dB
�B
�bB
{�B
t�B
n�B
ZB
Q�B
E�B
D�B
?}B
8RB
'�B
�B
JB	��B	��B	�B	�fB	�fB	�BB	��B	�9B	�3B	�FB	��B	�=B	�B	jB	t�B	`BB	S�B	S�B	N�B	H�B	<jB	<jB	:^B	0!B	�B	B��B��B��B�B��B��B�B�/B�B��BɺBĜB�RB�XB�qB�qB�^B�LB�9B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�PB�DB�PB�DB�=B�1B�%B�+B�1B�1B�=B�+B�%B�1B�%B�B�1B�7B�1B�+B�B�B� B� B~�B~�B|�B{�B{�Bz�Bz�Bx�Bw�Bw�By�Bx�Bx�B{�Bv�Bt�Bp�Bo�Bl�Bl�Bl�Bm�Bm�Bm�Bn�Bp�Bp�Bq�Br�Bs�Bs�Bs�Bu�Bw�Bz�B{�B}�B~�B� B�B�B�B�B�+B�1B�B�%B�%B�B�1B�VB�bB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�9B�?B�?B�FB�RB�^B�dB�qB�jB�qB�wB�}BBŢBǮBɺB��B��B��B�
B�B�)B�/B�BB�`B�B�B�B��B��B	B	%B	JB	\B	oB	{B	{B	uB	hB	bB	JB	oB	bB	hB	hB	VB	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	%�B	&�B	&�B	&�B	&�B	(�B	,B	-B	.B	.B	/B	0!B	1'B	1'B	49B	6FB	:^B	<jB	A�B	C�B	D�B	E�B	G�B	H�B	K�B	L�B	N�B	P�B	P�B	S�B	YB	ZB	[#B	\)B	^5B	bNB	cTB	ffB	iyB	k�B	m�B	n�B	p�B	q�B	s�B	t�B	t�B	u�B	v�B	v�B	w�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�RB	�jB	�jB	�qB	��B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	�B	�B	�#B	�#B	�#B	�)B	�;B	�BB	�BB	�HB	�TB	�NB	�NB	�`B	�mB	�mB	�mB	�mB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�ZB	�NB	�HB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447402012010314474020120103144740  AO  ARGQ                                                                        20111130143334  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143334  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144740  IP                  G�O�G�O�G�O�                