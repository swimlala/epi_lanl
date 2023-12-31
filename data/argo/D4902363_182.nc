CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-24T00:35:20Z creation;2017-11-24T00:35:24Z conversion to V3.1;2019-12-19T07:55:56Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171124003520  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_182                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�7�$� 1   @�7���� @;��7Kƨ�daXbM�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @���@���A   A>ffA`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�C3D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�
=@�=qA�RA9�AZ�RA|Q�A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.G�B6�B>�BF�BN�BV�B^�Bg{Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C�C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?�HD@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl�{Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�2=D�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�x�D��qD��qD�8�D�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD���D��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�5qD�uqD׵qD��qD�5qD�uqDصqD���D�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�x�DߵqD��qD�8�D�x�D�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��=D�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�8�D�o
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�C�A�E�A�Q�A�VA�^5A�dZA�n�A�r�A�t�A�t�A�r�A�n�A�x�A�t�A�v�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�v�A�z�A�|�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�jA�`BA���A�`BA�bNA��A��A���A��PA���A��A��+A��DA�ȴA���A���A��A��uA�r�A��DA�1A��9A���A��RA�dZA�~�A���A�A�A��/A�jA�A�dZA�  A�ĜA��PA���A���A�ȴA�ĜA�VA�t�A��A��A�^5AC�A~{A}��A}\)A|��A{�#Az��Az(�Ay�
Ay��Ay��Ay"�Ax�DAxAwS�Av�9Av  Au�^Au�Ar  AqS�ApĜApbNAp �Ao?}AlĜAlbAkK�Aj��Aj �Ai�FAi��Ai
=Ah(�Ag��Af��Ae�-AeoAdZAc��Ac?}AbĜAa�#Aax�Aa/A`v�A_��A_�A^�/A]��A\1AZ�RAYK�AX��AX�DAW�FAV5?ATn�AS�AS\)AR��AQ�TAP��AO��AN^5AMS�AM/ALȴAK��AK
=AJ�\AI`BAHffAG�wAG�AE�mAD�AC�ABbNA@�RA@5?A?��A>�`A>�A>z�A=ƨA=K�A<�uA<E�A;A:�+A9VA6�HA6=qA6{A5�A5�PA4��A3�mA2�+A1��A1x�A17LA0n�A.�HA,��A,1'A,{A+�#A+�7A*��A*$�A)33A(��A(ĜA(E�A'��A'ƨA'x�A'�A&n�A$�`A$^5A$A#A#�PA#�A"��A"  A!�PA!�A��AS�A�AjA(�A�A33A��AXA��A�jAz�A;dAdZA�/AVA7LAS�A��A��AoA�A�AdZA
VA	ƨA	
=A�uA��A�^Ap�AZAt�AhsA�AjAK�A-A ��A ��A ff@���@�^5@�{@���@�x�@�G�@���@�|�@���@��`@�=q@�^5@���@�Z@���@���@�w@�@�j@�F@�@�%@�t�@���@�M�@�I�@ݩ�@�A�@��@��;@�o@�x�@���@��@�|�@�v�@�%@ϕ�@�ȴ@�J@�G�@�9X@��;@ˮ@�S�@��y@ʇ+@�9X@�hs@þw@�+@��-@�b@�V@�%@��j@�r�@�b@�t�@�$�@��@�1'@�~�@���@�t�@�"�@�V@�x�@�7L@��9@� �@�@�S�@��\@�{@���@�V@���@�V@���@��@��`@��/@���@���@�@��!@�@�1@�S�@�1@�;d@�G�@�/@��u@��P@�ȴ@�hs@�O�@�`B@��@�r�@���@��@��@�b@�ƨ@�|�@�dZ@�l�@��R@�$�@���@�@��@�p�@�hs@�hs@�O�@��@�V@���@��j@��@�9X@��m@���@�t�@�"�@��!@�@�?}@�G�@�hs@�X@�V@�V@��@��u@�z�@�|�@��@���@��!@��\@��\@�=q@��@�O�@�%@��j@��@�Z@���@�t�@�o@�n�@�x�@�r�@�9X@���@��@���@�M�@�-@��T@�7L@���@��@��`@�Ĝ@��u@�I�@��@�ƨ@���@�l�@�K�@���@�^5@�$�@��T@��-@��@�X@�7L@��`@��@�bN@�b@+@~��@~V@}@}�@}`B@}V@|�@|�j@|z�@|1@{@{@{"�@{"�@{@{@{"�@z��@z^5@y�#@yX@xQ�@w��@w�@w�P@w�w@w�@w�P@w|�@w�@vE�@t��@r�@r-@rJ@r-@r^5@r�H@r�!@rM�@r=q@r�@o��@nȴ@n�+@nE�@nȴ@n��@n�+@m�@m�-@m��@m@m`B@mO�@mV@l��@lZ@l�@l9X@k��@kC�@k@j��@j~�@j�H@k33@j�!@j~�@j=q@i��@iG�@hĜ@h�u@h�@h�@h�u@h�u@h�@hr�@hbN@hr�@hr�@hQ�@g|�@g
=@fE�@e�@e��@e�h@ep�@d��@d�@dz�@dI�@d�@c�
@c��@c�m@d�@d��@d�/@d��@d��@d��@d�@d�@d�/@d�@d��@d�@d�j@d��@d��@d�@c�m@c��@ct�@cS�@co@bn�@b�@a�@a��@a��@aX@`��@`bN@_l�@_;d@^v�@]�@]p�@]/@\�/@[��@[��@[t�@Z~�@Z=q@Z�@Y��@Y�^@Y��@Yx�@YX@Y�@X�9@XbN@X  @W�w@W��@W��@W��@W��@W�@W�P@W\)@Vff@U�T@U�h@U`B@U�@T�/@Tz�@So@RM�@R-@R-@R-@Q�7@P�9@PQ�@Pb@P �@P �@P �@P  @O��@N�@N@M`B@MV@L�@L1@Kt�@Ko@KS�@K��@K��@Kt�@J��@I�@I��@H�`@H�u@G�@G�@F�y@Fff@F@E�-@EV@D��@C�
@CdZ@C33@C"�@B��@B��@B~�@B~�@Bn�@BM�@B=q@B-@B�@B�@B�@BJ@A�@A�7@AX@A%@@Ĝ@@A�@@b@?��@?�w@?�w@?�P@?|�@?|�@?\)@?\)@?K�@?;d@?;d@?;d@?;d@>��@>ȴ@>��@>��@>5?@=��@=@=��@=�@=/@<��@<Z@;S�@;o@:�H@:�\@:~�@:M�@:�@9��@9��@9�@8��@8�9@8��@7�w@7K�@6�+@6ff@65?@5�T@5��@5�@5?}@5�@5V@4�@4��@4��@4j@4Z@4�@3ƨ@3��@3dZ@3"�@3@2��@2��@2~�@2M�@2-@2�@1�7@1&�@0�@01'@/K�@/�@.�y@.�y@.�@.�R@.��@.��@.ff@-�@-@-p�@-/@,��@,�j@,�@,j@,�@+�F@+t�@+S�@+"�@*�H@*~�@)�@)��@)��@)��@)��@)&�@(�9@(��@(��@(�@(bN@(b@'|�@'
=@&��@&v�@&5?@%�@%�h@$��@$�@$z�@$j@$Z@$9X@$(�@$�@#��@#t�@"�@"�\@"=q@!�@!��@!��@!X@!G�@!G�@!&�@ ��@ �9@ bN@ Q�@ Q�@ bN@ bN@ A�@�;@��@K�@�R@V@�@@�-@`B@��@�D@j@j@j@Z@I�@�@�m@��@dZ@@�H@��@�\@~�@M�@��@�@�`@Ĝ@�u@�u@�@bN@Q�@1'@��@l�@
=@��@@@��@��@?}@�@��@�j@��@��@I�@(�@�@�
@�@dZ@C�@@��@~�@^5@M�@-@��@�#@�^@�^@��@��@�^@�^@�^@��@x�@X@7L@&�@&�@%@�`@Ĝ@��@ �@b@  @��@�P@l�@;d@��@�@�@ȴ@ȴ@��@�+@ff@@@@�-@�h@O�@�@�/@j@�@��@��@�
@��@��@dZ@33@"�@
�@
�!@
~�@
M�@
=q@
-@
J@	��@	��@	hs@	7L@��@��@�@bN@A�@b@�;@��@\)@K�@;d@�@�y@��@v�@5?@�T@@�-@�-@p�@��@�@�@z�@j@Z@Z@I�@1@�F@��@��@�@t�@t�@dZ@dZ@S�@S�@C�@"�@�H@��@~�@M�@=q@J@��@��@��@x�@&�@ �`@ �`@ ��@ Ĝ@ ��@ ��@ �u@ �@ r�@ �11111111111111111111111111111111111144111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A�C�A�E�A�Q�A�VA�^5A�dZA�n�A�r�A�t�A�t�A�r�A�n�A�x�A�t�A�v�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�r�A�v�A�z�A�|�A�x�A�x�A�v�A�v�A�v�A�v�A�v�A�v�A�v�G�O�G�O�A�`BA���A�`BA�bNA��A��A���A��PA���A��A��+A��DA�ȴA���A���A��A��uA�r�A��DA�1A��9A���A��RA�dZA�~�A���A�A�A��/A�jA�A�dZA�  G�O�G�O�A���A���A�ȴA�ĜA�VA�t�A��A��A�^5AC�A~{A}��A}\)A|��A{�#Az��Az(�Ay�
Ay��Ay��Ay"�Ax�DAxAwS�Av�9Av  Au�^Au�Ar  AqS�ApĜApbNAp �Ao?}AlĜAlbAkK�Aj��Aj �Ai�FAi��Ai
=Ah(�Ag��Af��Ae�-AeoAdZAc��Ac?}AbĜAa�#Aax�Aa/A`v�A_��A_�A^�/A]��A\1AZ�RAYK�AX��AX�DAW�FAV5?ATn�AS�AS\)AR��AQ�TAP��AO��AN^5AMS�AM/ALȴAK��AK
=AJ�\AI`BAHffAG�wAG�AE�mAD�AC�ABbNA@�RA@5?A?��A>�`A>�A>z�A=ƨA=K�A<�uA<E�A;A:�+A9VA6�HA6=qA6{A5�A5�PA4��A3�mA2�+A1��A1x�A17LA0n�A.�HA,��A,1'A,{A+�#A+�7A*��A*$�A)33A(��A(ĜA(E�A'��A'ƨA'x�A'�A&n�A$�`A$^5A$A#A#�PA#�A"��A"  A!�PA!�A��AS�A�AjA(�A�A33A��AXA��A�jAz�A;dAdZA�/AVA7LAS�A��A��AoA�A�AdZA
VA	ƨA	
=A�uA��A�^Ap�AZAt�AhsA�AjAK�A-A ��A ��A ff@���@�^5@�{@���@�x�@�G�@���@�|�@���@��`@�=q@�^5@���@�Z@���@���@�w@�@�j@�F@�@�%@�t�@���@�M�@�I�@ݩ�@�A�@��@��;@�o@�x�@���@��@�|�@�v�@�%@ϕ�@�ȴ@�J@�G�@�9X@��;@ˮ@�S�@��y@ʇ+@�9X@�hs@þw@�+@��-@�b@�V@�%@��j@�r�@�b@�t�@�$�@��@�1'@�~�@���@�t�@�"�@�V@�x�@�7L@��9@� �@�@�S�@��\@�{@���@�V@���@�V@���@��@��`@��/@���@���@�@��!@�@�1@�S�@�1@�;d@�G�@�/@��u@��P@�ȴ@�hs@�O�@�`B@��@�r�@���@��@��@�b@�ƨ@�|�@�dZ@�l�@��R@�$�@���@�@��@�p�@�hs@�hs@�O�@��@�V@���@��j@��@�9X@��m@���@�t�@�"�@��!@�@�?}@�G�@�hs@�X@�V@�V@��@��u@�z�@�|�@��@���@��!@��\@��\@�=q@��@�O�@�%@��j@��@�Z@���@�t�@�o@�n�@�x�@�r�@�9X@���@��@���@�M�@�-@��T@�7L@���@��@��`@�Ĝ@��u@�I�@��@�ƨ@���@�l�@�K�@���@�^5@�$�@��T@��-@��@�X@�7L@��`@��@�bN@�b@+@~��@~V@}@}�@}`B@}V@|�@|�j@|z�@|1@{@{@{"�@{"�@{@{@{"�@z��@z^5@y�#@yX@xQ�@w��@w�@w�P@w�w@w�@w�P@w|�@w�@vE�@t��@r�@r-@rJ@r-@r^5@r�H@r�!@rM�@r=q@r�@o��@nȴ@n�+@nE�@nȴ@n��@n�+@m�@m�-@m��@m@m`B@mO�@mV@l��@lZ@l�@l9X@k��@kC�@k@j��@j~�@j�H@k33@j�!@j~�@j=q@i��@iG�@hĜ@h�u@h�@h�@h�u@h�u@h�@hr�@hbN@hr�@hr�@hQ�@g|�@g
=@fE�@e�@e��@e�h@ep�@d��@d�@dz�@dI�@d�@c�
@c��@c�m@d�@d��@d�/@d��@d��@d��@d�@d�@d�/@d�@d��@d�@d�j@d��@d��@d�@c�m@c��@ct�@cS�@co@bn�@b�@a�@a��@a��@aX@`��@`bN@_l�@_;d@^v�@]�@]p�@]/@\�/@[��@[��@[t�@Z~�@Z=q@Z�@Y��@Y�^@Y��@Yx�@YX@Y�@X�9@XbN@X  @W�w@W��@W��@W��@W��@W�@W�P@W\)@Vff@U�T@U�h@U`B@U�@T�/@Tz�@So@RM�@R-@R-@R-@Q�7@P�9@PQ�@Pb@P �@P �@P �@P  @O��@N�@N@M`B@MV@L�@L1@Kt�@Ko@KS�@K��@K��@Kt�@J��@I�@I��@H�`@H�u@G�@G�@F�y@Fff@F@E�-@EV@D��@C�
@CdZ@C33@C"�@B��@B��@B~�@B~�@Bn�@BM�@B=q@B-@B�@B�@B�@BJ@A�@A�7@AX@A%@@Ĝ@@A�@@b@?��@?�w@?�w@?�P@?|�@?|�@?\)@?\)@?K�@?;d@?;d@?;d@?;d@>��@>ȴ@>��@>��@>5?@=��@=@=��@=�@=/@<��@<Z@;S�@;o@:�H@:�\@:~�@:M�@:�@9��@9��@9�@8��@8�9@8��@7�w@7K�@6�+@6ff@65?@5�T@5��@5�@5?}@5�@5V@4�@4��@4��@4j@4Z@4�@3ƨ@3��@3dZ@3"�@3@2��@2��@2~�@2M�@2-@2�@1�7@1&�@0�@01'@/K�@/�@.�y@.�y@.�@.�R@.��@.��@.ff@-�@-@-p�@-/@,��@,�j@,�@,j@,�@+�F@+t�@+S�@+"�@*�H@*~�@)�@)��@)��@)��@)��@)&�@(�9@(��@(��@(�@(bN@(b@'|�@'
=@&��@&v�@&5?@%�@%�h@$��@$�@$z�@$j@$Z@$9X@$(�@$�@#��@#t�@"�@"�\@"=q@!�@!��@!��@!X@!G�@!G�@!&�@ ��@ �9@ bN@ Q�@ Q�@ bN@ bN@ A�@�;@��@K�@�R@V@�@@�-@`B@��@�D@j@j@j@Z@I�@�@�m@��@dZ@@�H@��@�\@~�@M�@��@�@�`@Ĝ@�u@�u@�@bN@Q�@1'@��@l�@
=@��@@@��@��@?}@�@��@�j@��@��@I�@(�@�@�
@�@dZ@C�@@��@~�@^5@M�@-@��@�#@�^@�^@��@��@�^@�^@�^@��@x�@X@7L@&�@&�@%@�`@Ĝ@��@ �@b@  @��@�P@l�@;d@��@�@�@ȴ@ȴ@��@�+@ff@@@@�-@�h@O�@�@�/@j@�@��@��@�
@��@��@dZ@33@"�@
�@
�!@
~�@
M�@
=q@
-@
J@	��@	��@	hs@	7L@��@��@�@bN@A�@b@�;@��@\)@K�@;d@�@�y@��@v�@5?@�T@@�-@�-@p�@��@�@�@z�@j@Z@Z@I�@1@�F@��@��@�@t�@t�@dZ@dZ@S�@S�@C�@"�@�H@��@~�@M�@=q@J@��@��@��@x�@&�@ �`@ �`@ ��@ Ĝ@ ��@ ��@ �u@ �@ r�@ �11111111111111111111111111111111111144111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB  B��B��B�NB�PBƨB�B�BŢB�3B�FB�!B��B�DB�+B�1B�%Bx�Bp�BgmB^5BS�BK�BG�B6FB+B�BoBoBbB	7B%BB
��B
�B
�B
�B
�sB
�LB
�)B
��B
B
�RB
�3B
�B
��B
��B
�bB
�=B
�1B
�%B
�B
z�B
v�B
r�B
q�B
q�B
o�B
k�B
ffB
cTB
]/B
ZB
S�B
O�B
F�B
/B
33B
2-B
/B
)�B
 �B
\B
\B
JB
	7B
1B
%B
%B	��B	��B	��B	�B	�mB	�sB	�fB	�BB	�BB	�)B	�
B	�
B	��B	��B	ɺB	ȴB	��B	�RB	��B	��B	��B	��B	��B	��B	�DB	�B	�%B	�B	}�B	u�B	n�B	e`B	cTB	_;B	bNB	^5B	VB	R�B	Q�B	I�B	D�B	B�B	>wB	6FB	1'B	-B	#�B	�B	�B	�B	�B	�B	�B	uB	oB	VB	PB	1B��B��B�B��B��B�B�B�yB�ZB�/B�#B�B�
B��BŢB�wB��BŢBB�}B�dB�FB�9B�FB�FB�-B�'B�!B�B��B��B��B��B��B��B��B��B��B�{B�hB�\B�Bv�B�B�7B�1B�B�Bz�B� B�B~�B|�Bq�BgmBm�BjB`BBR�BK�BL�BR�BT�BS�BQ�BM�BK�BK�BL�BJ�BL�BJ�BB�BA�BG�BE�B=qB5?B2-B5?B?}BA�B@�BF�BJ�BJ�BI�BH�BG�BB�BD�B>wB9XB>wB?}B8RB/B0!B+B$�B#�B#�B �B#�B%�B�B$�B'�B"�B-B5?B49B/B+B)�B%�B/B2-B0!B1'B6FB7LB7LB6FB<jB=qB=qB=qB;dB33B/B9XB=qB9XB7LB9XB>wBH�BI�BG�BE�BB�BA�BE�BA�B>wBC�BI�BH�BH�BM�BK�BK�BF�BG�BXB^5BcTBffBjBp�Bq�Bs�Bs�Br�Bs�Bu�B|�B~�Bz�Bv�By�B�1B�=B�B�JB�=B�B�B�B�+B�VB�oB�\B�=B�B�JB�bB�uB��B��B��B��B��B��B��B�B�-B�9B�?B�FB�^B�jB�wBBŢBȴB��B��B��B��B��B��B��B��B�
B�B�B�B�#B�;B�;B�/B�`B�B�B�B��B��B�B��B��B��B��B��B�B�B�B�B�B�B��B��B	B	B	B	%B	+B	+B	DB	PB	VB	PB	VB	\B	bB	uB	{B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	&�B	&�B	(�B	)�B	-B	1'B	49B	49B	6FB	:^B	;dB	;dB	=qB	>wB	>wB	>wB	?}B	E�B	I�B	J�B	J�B	K�B	L�B	L�B	M�B	M�B	Q�B	XB	[#B	_;B	`BB	cTB	dZB	e`B	e`B	dZB	cTB	dZB	dZB	jB	m�B	p�B	r�B	s�B	u�B	v�B	w�B	v�B	t�B	v�B	|�B	}�B	~�B	� B	�B	�B	�%B	�=B	�=B	�=B	�PB	�JB	�PB	�PB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�9B	�?B	�9B	�9B	�9B	�9B	�FB	�LB	�RB	�RB	�LB	�RB	�XB	�^B	�dB	�qB	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�BB	�BB	�;B	�NB	�NB	�TB	�ZB	�`B	�`B	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
%B
B
B
%B
B
B
B
B
%B
%B
%B
+B
%B
+B
+B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
bB
hB
hB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
)�B
+B
.B
.B
/B
/B
.B
/B
/B
.B
.B
/B
/B
0!B
1'B
1'B
2-B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
5?B
6FB
6FB
6FB
6FB
8RB
:^B
:^B
9XB
9XB
8RB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
=qB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
F�B
F�B
G�B
H�B
G�B
G�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
O�B
O�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
XB
YB
YB
ZB
ZB
YB
YB
ZB
ZB
YB
[#B
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
^5B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
ffB
e`B
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�11111111111111111111111111111111111144111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB-BB-BB3BB-B-B-B-B-B-B-B-B-B-B-B-B-B-B-B-B-B-B-B'B'B'B'B'B;BUB �B�cB�G�O�G�O�B��B��B�#B�^B��B��B��B�B��B�rB��B�KB{�Bs3BjB`�BV�BN"BI�B9�B./B!�B�B{B�B
�BEBAB
�"B
�9B
�B
�wG�O�G�O�B
�IB
�hB
�B
�DB
��B
��B
��B
��B
� B
��B
��B
��B
�B
|6B
w�B
s�B
rB
q�B
p;B
lWB
gmB
d&B
^5B
[	B
T�B
P�B
HB
2GB
4B
2�B
/�B
*�B
"�B
 B
}B
PB

=B
�B
�B
�B	��B	�	B	��B	�B	��B	�_B	�B	�HB	��B	��B	�EB	רB	ԕB	��B	��B	�lB	ªB	�B	�_B	��B	�vB	��B	��B	�B	�PB	�B	��B	�B	B	w2B	p!B	g8B	d�B	`�B	b�B	_B	WsB	S�B	R�B	K^B	FB	C�B	?�B	8B	2�B	.�B	%�B	�B	�B	�B	�B	=B	KB	�B	@B	\B	B		lB	  B��B�B��B�+B�TB�vB�B�B�B�B��B��B�TB��B��B��B��B�-B�OB�jB��B��B��B��B�B��B��B��B��B�&B��B��B�vB�BB�OB��B�YB��B�TB��B�_By�B�-B��B��B�B�-B|�B��B��B�B}�Bs�Bi�Bn�Bk�BbhBU�BN�BNpBT,BU�BUBSBO\BL�BMBM�BK�BMjBK�BDBB�BHBFtB>�B6�B3�B6�B@BBABA�BGzBKBKBJ=BIBHfBC�BE�B?�B;B>�B@OB9�B0�B1B,=B&2B$�B$�B"4B%B'�B"B&fB)yB$�B-�B5�B4�B0!B,qB+�B(
B0;B3B1[B2aB6�B8B8B7B<�B=�B>B>(B<6B5B1'B:�B>]B:�B8�B:�B?}BIBJ#BHKBFtBC�BB�BFtBB�B?�BD�BJ#BIlBI�BN<BL~BL�BHfBI�BX�B^�Bc�BgBj�Bp�Bq�BtBtBsBtBv�B}�B�B{�BxBz�B�1B�)B��B��B�B�B��B�'B�zB��B��B�.B��B�YB��B��B��B�B��B�B�xB�TB�FB�B�QB�|B��B�tB��B��B��B��B��B�B�B�DB�VB�<B�<B�jB˒B�VB� B�$B�_B�yB�kBیB�pBߤB�B��B��B��B��B��B�?B�TB�2B�2B�2B�2B�FB�9B�3B�9B�B�B�B�>B�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	
B		B	!B	!-B	# B	$&B	&2B	'8B	'mB	)DB	*B	-wB	1�B	4�B	4�B	6�B	:�B	;�B	;�B	=�B	>�B	>�B	>�B	@ B	E�B	I�B	J�B	KB	K�B	MB	M6B	N<B	NVB	RoB	X�B	[�B	_pB	`�B	cnB	d�B	e�B	e�B	d�B	c�B	eB	eB	j�B	m�B	p�B	r�B	s�B	u�B	w2B	xB	w2B	u�B	wLB	}<B	~(B	B	�4B	�oB	�[B	�YB	�XB	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�@B	�FB	�>B	�DB	�WB	�IB	�OB	�OB	�;B	�oB	�oB	�vB	�nB	�ZB	��B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�:B	�2B	�?B	�kB	�B	�]B	�pB	ߊB	ߤB	ߊB	��B	�B	�B	�B	�B	�B	�B	ߤB	�B	��B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	� B	�/B	��B	��B	��B	��B	�B	�B	�AB	��B	��B	��B	�B	�FB	�%B	��B	�B	�	B	�B	�B	�B	�PB	�JB	�jB	�<B	�VB	�VB	�<B	�VB	�HB
'B
B
tB
tB
�B
�B
tB
�B
�B
�B
{B
tB
�B
�B
zB
�B
zB
�B
	�B

�B
�B
�B
xB
�B
dB
~B
~B
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
B
�B
B
=B
�B
B
�B
�B
B
B
B
!B
!B
 B
!B
 �B
 BB
 B
 'B
!�B
"B
!�B
# B
#B
$B
$&B
%B
%B
%B
%,B
%,B
%B
%,B
%,B
&2B
&2B
&2B
'8B
'B
(>B
(>B
(>B
(>B
(>B
'RB
(XB
(>B
*eB
+�B
.IB
.IB
/OB
/OB
.IB
/5B
/iB
.cB
.cB
/iB
/iB
0UB
1[B
1vB
2|B
1vB
1[B
1�B
2|B
3hB
3hB
3hB
3�B
3�B
5�B
6zB
6zB
6zB
6�B
8�B
:xB
:�B
9�B
9�B
8�B
7�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
;�B
<�B
=�B
>�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
EB
D�B
F�B
F�B
G�B
IB
G�B
HB
IB
J�B
J�B
J�B
J�B
KB
J�B
J�B
KB
K�B
K�B
MB
MB
MB
MB
MB
LB
MB
O(B
O(B
P.B
PB
PB
PB
P.B
PB
OBB
PHB
PHB
QNB
Q4B
R:B
S@B
S&B
S@B
S&B
TB
T,B
T,B
UB
T,B
UMB
UMB
U2B
UMB
VSB
V9B
VSB
V9B
W?B
WYB
XEB
XEB
X_B
X_B
X_B
YKB
YKB
Y1B
YKB
Y1B
YKB
YKB
X_B
YKB
YeB
ZQB
Z7B
YKB
YeB
ZkB
ZkB
YB
[WB
[qB
ZkB
[WB
[qB
[qB
\xB
\]B
\]B
\]B
\CB
\xB
\]B
\xB
\xB
\xB
^OB
]dB
]~B
]dB
]~B
^�B
^�B
_pB
`vB
`vB
`�B
`�B
`�B
`�B
`�B
abB
`vB
`vB
a�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
g�B
g�B
f�B
e�B
h�B
h�B
h�B
h�B
h�B
i�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�11111111111111111111111111111111111144111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<� /<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.33(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711280034292017112800342920171128003429201806221233562018062212335620180622123356201804050429582018040504295820180405042958  JA  ARFMdecpA19c                                                                20171124093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171124003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171124003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171124003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171124003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171124003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171124003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20171124003524  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20171124003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171124003524  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20171124003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171124003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20171124005620                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171124153346  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171125000000  CF  PSAL_ADJUSTED_QCC  C  G�O�                JM  ARSQJMQC2.0                                                                 20171125000000  CF  TEMP_ADJUSTED_QCC  C  G�O�                JM  ARCAJMQC2.0                                                                 20171127153429  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171127153429  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192958  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033356  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                