CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-27T00:35:40Z creation;2016-09-27T00:35:42Z conversion to V3.1;2019-12-19T08:29:25Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160927003540  20200115111518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA  I2_0576_041                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���_� 1   @���hK� @;&�����du\(�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @w�@�=q@�p�A�RA9�AZ�RAz�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�(�B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B��=B�W
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
C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP�{DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVqHDV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw��Dxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��=D��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD���D���D�5qD�uqD��qD��qD�5qD�uqD���D��qD�5qD�uqD��qD���D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqDµqD��qD�5qD�uqDõqD��qD�5qD�uqDĵqD��qD�5qD�uqDŵqD��qD�5qD�uqDƵqD��qD�5qD�uqDǵqD��qD�5qD�uqDȵqD��qD�5qD�uqDɵqD��qD�5qD�uqDʵqD��qD�5qD�uqD˵qD��qD�5qD�uqD̵qD��qD�5qD�uqD͵qD��qD�5qD�uqDεqD��qD�5qD�uqDϵqD��qD�5qD�uqDеqD��qD�5qD�uqDѵqD��qD�5qD�uqDҵqD��qD�5qD�uqDӵqD��qD�5qD�uqDԵqD��qD�5qD�uqDյqD��qD�5qD�uqDֵqD��qD�2=D�r=D׵qD��qD�5qD�uqDصqD��qD�5qD�uqDٵqD��qD�5qD�uqDڵqD��qD�5qD�uqD۵qD��qD�5qD�uqDܵqD��qD�5qD�uqDݵqD��qD�5qD�uqD޵qD��qD�5qD�uqDߵqD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD�qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�E�A�G�A�I�A�K�A�I�A�G�A�E�A�I�A�K�A�9XA�5?A�1'A���AӺ^A�XA�S�A�dZA�&�AìA�\)A�I�A��uA���A�p�A�^5A��!A�A�=qA�|�A��A���A��DA��uA�A���A���A�5?A�VA���A��A�hsA���A�?}A���A�=qA��`A�~�A� �A�v�A�M�A�33A�$�A���A��-A�  A�?}A�ĜA���A�O�A�(�A�A�G�A�\)A��^A�~�A��yA��wA��A�bNA���A�G�A;dA}�Az=qAy�Ax�Av1AtjAs�Ar�ArJAp1AohsAn��Am%Ak\)Aj��AihsAi?}Ah^5Ag"�Ae�AdjAa��AaO�Aa�A`��A`�/A`�A`��A`A]�
A\n�AZ��AY�FAY�AX�RAXVAW�AV9XAT�AS&�AS&�AR��AQ�#AQ;dAP��AP�RAP�+AOp�AL�AL$�AL�AK�AKp�AI�AG�TAF�HAF^5AEl�AD��AC�7AB�uAA�A@(�A?7LA?33A?�mA?�wA>ZA>^5A=�A;��A9A8�A7/A5�A4A�A3|�A3
=A1G�A1��A1��A1p�A/��A/t�A.  A,��A,I�A,  A+`BA+A*�A)O�A(�A'�7A&��A&�\A%�A$�HA$=qA#�A"ĜA"9XA!�;A!�-A!��A!�hA!l�A �/A bA33AĜA��A�DA�+Ar�A�
A?}A��A��A��A�A;dAoA��A��A�/A�AjA1A�HAbA�hAdZAr�A�
At�A��A9XA�/AbA�
A"�A�A�mAoA~�A=qA�TA�A	hsA��A-A&�AVA�A�mA��A�^A�A�Al�A �A �\@�l�@��h@�ƨ@�@�G�@�K�@�M�@�p�@�l�@�$�@���@��@�t�@��@���@�@�t�@⟾@��T@�?}@�b@ާ�@�@�(�@�
=@��#@׾w@�K�@�n�@�t�@�ff@��@ύP@͡�@�Ĝ@�A�@��m@˥�@�ȴ@���@�1'@�~�@�hs@ě�@��m@�C�@�ȴ@��#@�1@���@��@�?}@���@�(�@��R@�?}@���@�/@�dZ@�E�@���@��h@�V@��D@�I�@��P@��@�@�/@��@� �@��w@�"�@��R@�n�@��T@�X@��@��/@�j@��@��H@�V@��@�1'@��m@�l�@��R@��#@��`@�Q�@��F@�"�@���@���@��@�?}@���@�Z@��;@�"�@��@�&�@�Q�@� �@�(�@��;@��+@��T@���@��-@���@��h@�Ĝ@�j@�A�@�  @�;d@��\@�V@��@�@�@��@�5?@�ff@�ff@�ff@�E�@�`B@��j@���@���@��@��j@�(�@�|�@�ff@�n�@�|�@�C�@�\)@��;@�dZ@���@�v�@�~�@��@�
=@��y@�V@�n�@�E�@�$�@�$�@�-@�{@��#@���@�`B@�?}@��@���@���@���@�M�@��@���@���@��@��@�Q�@���@��@�n�@���@���@��+@�=q@�{@�{@�E�@�E�@�E�@�=q@�$�@���@�O�@�O�@�7L@�V@�z�@��F@�l�@�K�@��@�@�`B@��@��@�w@�P@~v�@z�@{��@~ȴ@}�-@~$�@~$�@|��@|�@|j@|�@{t�@{@z��@|1@}?}@|�D@|9X@|1@{�
@{t�@{C�@{"�@{o@z��@z��@z��@z�!@z��@zM�@zM�@zM�@z�@y��@y�7@yhs@y&�@y�@y%@x�u@xA�@x1'@x �@x  @w�P@w;d@vȴ@vE�@u`B@uO�@u`B@u`B@u?}@t��@s�
@s@r�H@r��@r��@r�!@rn�@rM�@r-@q�#@qx�@q7L@q7L@qhs@qG�@p��@pĜ@pĜ@p�9@o��@oK�@o;d@nff@m�T@m�h@m�@m/@l�/@l�j@l��@l�@kdZ@k"�@j�!@j�@i�#@iG�@h�@g�;@g�P@gl�@gK�@f�+@f�R@f�y@f�@f��@f5?@f{@e��@eV@d�@dz�@d�@c�F@c�@b��@b=q@bJ@a��@ax�@ahs@aG�@a�@`�@`1'@`b@_�@_\)@_
=@^�+@^5?@\�@\I�@\�@[�F@[t�@[o@Z�H@Z�!@ZM�@Y��@Y�7@Y�@X�`@XĜ@XQ�@Xb@W�;@W��@V�y@Vff@VE�@U��@U�-@Up�@UV@TI�@Sƨ@R��@Q��@Qx�@P�`@PĜ@P��@Pr�@PA�@Pb@P  @O�@O�;@O�P@O�P@O|�@O\)@N��@Nff@M�T@M�-@M��@M��@Mp�@M/@L��@Lz�@L�@K�
@K��@K"�@K@J�@J��@J��@J�\@Jn�@J^5@JM�@J=q@J�@I�^@I��@I��@I��@I�7@Ihs@I%@H��@H�9@Hr�@HA�@G�;@G�P@G\)@G
=@F��@E�@E��@E��@E�-@Ep�@D��@Dj@D1@Ct�@B�!@B~�@BM�@A��@A��@A��@AG�@A�@A&�@A��@A�#@A7L@@r�@>ȴ@=��@=�-@=�h@=��@=��@=@=@>$�@=�-@=`B@<�@<�j@<j@<9X@;��@;33@;33@;@:��@:�!@:��@:��@:��@:�\@:~�@:=q@9��@9�@8�u@8Q�@8 �@8  @7��@7�@7l�@7;d@6�y@6�+@5��@5�-@5��@5`B@4�j@4Z@41@3��@3�@3dZ@3S�@333@2�!@2-@1�#@1��@1��@1hs@1X@1X@1X@1X@1G�@1�@0�`@0�@/�@/�w@/�P@.ff@.@-�-@-�h@-�h@-O�@-/@-�@-V@,�@,�/@,�@,9X@+�
@+��@+�@+33@*�H@*��@*�!@*^5@*�@)�@)��@)G�@(�9@(�@(bN@(Q�@( �@'�@'l�@';d@&ȴ@&�+@&v�@&ff@&V@&E�@&E�@&{@%�T@%��@%`B@$�@$�j@$�D@$I�@$�@#�@#o@"�!@"~�@"^5@"=q@"�@"J@!�@!�#@!��@!�^@!�^@!�^@!�^@!��@!�7@!�7@!x�@!%@ r�@ Q�@ 1'@  �@ b@�@�;@��@�@\)@��@V@$�@�@�h@O�@/@�@�@�D@I�@(�@(�@(�@1@�m@�
@ƨ@t�@33@��@�\@~�@~�@~�@~�@n�@n�@^5@=q@x�@G�@&�@�`@Ĝ@��@ �@b@b@  @�@�w@�@�@�P@|�@\)@+@��@��@�+@�h@p�@`B@?}@V@�@z�@�@�m@ƨ@�F@��@��@dZ@C�@o@��@��@~�@-@�@�^@hs@7L@�`@Ĝ@��@�@A�@1'@1'@b@  @�;@�@|�@|�@\)@K�@+@�@
=@�y@ȴ@��@��@��@ff@$�@�T@��@@��@�h@O�@V@��@�@�@�@�/@�j@z�@Z@Z@I�@(�@1@ƨ@�@dZ@"�@
��@
�\@
n�@
n�@
M�@
-@
�@	�#@	�^@	��@	hs@	&�@��@�`@Ĝ@�9@Q�@b@b@b@�@�w@l�@\)@;d@�@�@�@ff@E�@@��@�@p�@`B@?}@��@�/@�@I�@�
@�F@��@��@��@dZ@o@��@�!@�\@n�@�@�#@�^@��@��@�7@hs@X@X@G�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�E�A�G�A�I�A�K�A�I�A�G�A�E�A�I�A�K�A�9XA�5?A�1'A���AӺ^A�XA�S�A�dZA�&�AìA�\)A�I�A��uA���A�p�A�^5A��!A�A�=qA�|�A��A���A��DA��uA�A���A���A�5?A�VA���A��A�hsA���A�?}A���A�=qA��`A�~�A� �A�v�A�M�A�33A�$�A���A��-A�  A�?}A�ĜA���A�O�A�(�A�A�G�A�\)A��^A�~�A��yA��wA��A�bNA���A�G�A;dA}�Az=qAy�Ax�Av1AtjAs�Ar�ArJAp1AohsAn��Am%Ak\)Aj��AihsAi?}Ah^5Ag"�Ae�AdjAa��AaO�Aa�A`��A`�/A`�A`��A`A]�
A\n�AZ��AY�FAY�AX�RAXVAW�AV9XAT�AS&�AS&�AR��AQ�#AQ;dAP��AP�RAP�+AOp�AL�AL$�AL�AK�AKp�AI�AG�TAF�HAF^5AEl�AD��AC�7AB�uAA�A@(�A?7LA?33A?�mA?�wA>ZA>^5A=�A;��A9A8�A7/A5�A4A�A3|�A3
=A1G�A1��A1��A1p�A/��A/t�A.  A,��A,I�A,  A+`BA+A*�A)O�A(�A'�7A&��A&�\A%�A$�HA$=qA#�A"ĜA"9XA!�;A!�-A!��A!�hA!l�A �/A bA33AĜA��A�DA�+Ar�A�
A?}A��A��A��A�A;dAoA��A��A�/A�AjA1A�HAbA�hAdZAr�A�
At�A��A9XA�/AbA�
A"�A�A�mAoA~�A=qA�TA�A	hsA��A-A&�AVA�A�mA��A�^A�A�Al�A �A �\@�l�@��h@�ƨ@�@�G�@�K�@�M�@�p�@�l�@�$�@���@��@�t�@��@���@�@�t�@⟾@��T@�?}@�b@ާ�@�@�(�@�
=@��#@׾w@�K�@�n�@�t�@�ff@��@ύP@͡�@�Ĝ@�A�@��m@˥�@�ȴ@���@�1'@�~�@�hs@ě�@��m@�C�@�ȴ@��#@�1@���@��@�?}@���@�(�@��R@�?}@���@�/@�dZ@�E�@���@��h@�V@��D@�I�@��P@��@�@�/@��@� �@��w@�"�@��R@�n�@��T@�X@��@��/@�j@��@��H@�V@��@�1'@��m@�l�@��R@��#@��`@�Q�@��F@�"�@���@���@��@�?}@���@�Z@��;@�"�@��@�&�@�Q�@� �@�(�@��;@��+@��T@���@��-@���@��h@�Ĝ@�j@�A�@�  @�;d@��\@�V@��@�@�@��@�5?@�ff@�ff@�ff@�E�@�`B@��j@���@���@��@��j@�(�@�|�@�ff@�n�@�|�@�C�@�\)@��;@�dZ@���@�v�@�~�@��@�
=@��y@�V@�n�@�E�@�$�@�$�@�-@�{@��#@���@�`B@�?}@��@���@���@���@�M�@��@���@���@��@��@�Q�@���@��@�n�@���@���@��+@�=q@�{@�{@�E�@�E�@�E�@�=q@�$�@���@�O�@�O�@�7L@�V@�z�@��F@�l�@�K�@��@�@�`B@��@��@�w@�P@~v�@z�@{��@~ȴ@}�-@~$�@~$�@|��@|�@|j@|�@{t�@{@z��@|1@}?}@|�D@|9X@|1@{�
@{t�@{C�@{"�@{o@z��@z��@z��@z�!@z��@zM�@zM�@zM�@z�@y��@y�7@yhs@y&�@y�@y%@x�u@xA�@x1'@x �@x  @w�P@w;d@vȴ@vE�@u`B@uO�@u`B@u`B@u?}@t��@s�
@s@r�H@r��@r��@r�!@rn�@rM�@r-@q�#@qx�@q7L@q7L@qhs@qG�@p��@pĜ@pĜ@p�9@o��@oK�@o;d@nff@m�T@m�h@m�@m/@l�/@l�j@l��@l�@kdZ@k"�@j�!@j�@i�#@iG�@h�@g�;@g�P@gl�@gK�@f�+@f�R@f�y@f�@f��@f5?@f{@e��@eV@d�@dz�@d�@c�F@c�@b��@b=q@bJ@a��@ax�@ahs@aG�@a�@`�@`1'@`b@_�@_\)@_
=@^�+@^5?@\�@\I�@\�@[�F@[t�@[o@Z�H@Z�!@ZM�@Y��@Y�7@Y�@X�`@XĜ@XQ�@Xb@W�;@W��@V�y@Vff@VE�@U��@U�-@Up�@UV@TI�@Sƨ@R��@Q��@Qx�@P�`@PĜ@P��@Pr�@PA�@Pb@P  @O�@O�;@O�P@O�P@O|�@O\)@N��@Nff@M�T@M�-@M��@M��@Mp�@M/@L��@Lz�@L�@K�
@K��@K"�@K@J�@J��@J��@J�\@Jn�@J^5@JM�@J=q@J�@I�^@I��@I��@I��@I�7@Ihs@I%@H��@H�9@Hr�@HA�@G�;@G�P@G\)@G
=@F��@E�@E��@E��@E�-@Ep�@D��@Dj@D1@Ct�@B�!@B~�@BM�@A��@A��@A��@AG�@A�@A&�@A��@A�#@A7L@@r�@>ȴ@=��@=�-@=�h@=��@=��@=@=@>$�@=�-@=`B@<�@<�j@<j@<9X@;��@;33@;33@;@:��@:�!@:��@:��@:��@:�\@:~�@:=q@9��@9�@8�u@8Q�@8 �@8  @7��@7�@7l�@7;d@6�y@6�+@5��@5�-@5��@5`B@4�j@4Z@41@3��@3�@3dZ@3S�@333@2�!@2-@1�#@1��@1��@1hs@1X@1X@1X@1X@1G�@1�@0�`@0�@/�@/�w@/�P@.ff@.@-�-@-�h@-�h@-O�@-/@-�@-V@,�@,�/@,�@,9X@+�
@+��@+�@+33@*�H@*��@*�!@*^5@*�@)�@)��@)G�@(�9@(�@(bN@(Q�@( �@'�@'l�@';d@&ȴ@&�+@&v�@&ff@&V@&E�@&E�@&{@%�T@%��@%`B@$�@$�j@$�D@$I�@$�@#�@#o@"�!@"~�@"^5@"=q@"�@"J@!�@!�#@!��@!�^@!�^@!�^@!�^@!��@!�7@!�7@!x�@!%@ r�@ Q�@ 1'@  �@ b@�@�;@��@�@\)@��@V@$�@�@�h@O�@/@�@�@�D@I�@(�@(�@(�@1@�m@�
@ƨ@t�@33@��@�\@~�@~�@~�@~�@n�@n�@^5@=q@x�@G�@&�@�`@Ĝ@��@ �@b@b@  @�@�w@�@�@�P@|�@\)@+@��@��@�+@�h@p�@`B@?}@V@�@z�@�@�m@ƨ@�F@��@��@dZ@C�@o@��@��@~�@-@�@�^@hs@7L@�`@Ĝ@��@�@A�@1'@1'@b@  @�;@�@|�@|�@\)@K�@+@�@
=@�y@ȴ@��@��@��@ff@$�@�T@��@@��@�h@O�@V@��@�@�@�@�/@�j@z�@Z@Z@I�@(�@1@ƨ@�@dZ@"�@
��@
�\@
n�@
n�@
M�@
-@
�@	�#@	�^@	��@	hs@	&�@��@�`@Ĝ@�9@Q�@b@b@b@�@�w@l�@\)@;d@�@�@�@ff@E�@@��@�@p�@`B@?}@��@�/@�@I�@�
@�F@��@��@��@dZ@o@��@�!@�\@n�@�@�#@�^@��@��@�7@hs@X@X@G�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVBW
BW
BW
BVBXBYBZBZBYB_;B`BB`BBffBk�Bn�Bw�BhsBXBF�B6FB(�B
=B�B�BŢB�dB�B��B�hBv�Bo�BhsB_;BXBYBVBS�B@�B+BuBDBB��B��B�B�yB�TB�BȴB��B�PB�=B�%B�Bs�BT�BE�BC�B:^B.B�B	7BB
�sB
�yB
�BB
�#B
�B
B
��B
�uB
�oB
�B
n�B
iyB
ffB
^5B
R�B
M�B
E�B
A�B
49B
0!B
)�B
�B
DB
  B	��B	��B	�B	�sB	�)B	��B	�FB	�'B	�!B	�!B	�B	�B	�'B	�B	��B	�\B	�B	�B	�PB	�1B	�%B	�JB	}�B	l�B	[#B	]/B	]/B	VB	S�B	O�B	N�B	L�B	J�B	,B	&�B	%�B	#�B	 �B	{B	bB	
=B	+B	+B	+B��B�B�B�B�ZB�fB��B	B��B��B��B�`B��B��BɺBĜB��B�dBŢB�wBȴB��B��BȴBĜB�dB�'B�-B�?B�'B�B�B�B�B��B��B��B��B��B��B�{B�bB�PB�PB�JB�JB�DB�DB�7B�1B�B�B�B�B�B�B�B�B� B~�B{�Bz�Bz�By�By�Bx�Bt�Br�Bq�Bq�Bp�Bm�BjBl�Bm�BiyBiyBffBe`BaHB^5B]/B[#BYBXBT�BS�BP�BP�BM�BK�BF�BF�BC�B@�B?}B:^B9XB9XB9XB5?B6FB49B49B33B2-B/B-B,B+B(�B(�B&�B%�B$�B"�B�B�B�B�B�B�B{B{B{BuBoBhB\BVBPBDBJBPBDBJBJB\BbBbB\B\BbB\BVB\B\BbBhBhBhBhBuB{B{B�B{B�B�B�B�B�B�B!�B!�B!�B"�B"�B#�B$�B%�B&�B)�B+B-B-B.B0!B1'B2-B33B33B33B49B49B7LB<jB>wB?}B@�BB�BD�BE�BF�BK�BN�BN�BN�BN�BQ�BS�BT�BZBbNBgmBhsBhsBn�Bp�Bp�Bs�Bz�B~�B~�B~�B~�B}�B�B�%B�B�%B�1B�%B�B�%B�1B�DB�JB�{B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�9B�FB�RB�}BÖBŢBŢBƨB��B��B��B�B�)B�/B�BB�TB�`B�mB�sB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	+B	1B	JB	VB	\B	�B	�B	�B	�B	 �B	$�B	$�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	(�B	'�B	)�B	,B	,B	-B	.B	.B	+B	/B	=qB	D�B	H�B	N�B	L�B	L�B	K�B	K�B	J�B	J�B	K�B	Q�B	ZB	^5B	`BB	aHB	bNB	ffB	hsB	iyB	jB	o�B	o�B	p�B	r�B	u�B	x�B	y�B	y�B	z�B	{�B	|�B	}�B	� B	� B	� B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�9B	�?B	�?B	�LB	�LB	�RB	�XB	�^B	�^B	�qB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ɺB	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B
PB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
 �B
�B
�B
�B
�B
�B
 �B
 �B
#�B
%�B
&�B
%�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
XB
XB
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BVBW$BW$BW$BV9BX+BY1BZ7BZ7BYeB_pB`�Ba-Bg�Bn�Bv�B�uBr�B^�BO�B="B49BB�tB�dB��B�oB�B�hB��By�BtTBjB`vBX�BZBX_BX�BEmB.�B2B�B�B�]B��B��B��B�FB�5B��B�B��B��B�_B�-Bv�BVBF�BEB=<B0�B�B�B�B
�B
��B
�-B
ܬB
�=B
��B
��B
��B
��B
�YB
o�B
kQB
iDB
_�B
S�B
OBB
G+B
C�B
5ZB
1vB
,"B
~B
~B
oB	��B	�*B	�nB	�KB	ބB	�[B	�2B	��B	�oB	�oB	�iB	��B	��B	��B	��B	��B	�YB	��B	�pB	�B	�_B	�VB	� B	m�B	[�B	^B	^jB	V�B	T�B	P}B	O�B	N�B	M�B	,�B	'RB	&�B	%,B	#:B	�B	�B	^B	�B	KB	�B��B�ZB�[B�B�tB�2B��B	[B��B�$B��B��B��B��B˒BƨB��B��B�_B�wB�B̘BΊB��BƎB�"B��B��B�+B�B��B�OB�!B��B��B��B�B�TB��B��B��B�4B��B��B��B��B��B�dB�rB�RB��B�mB�gB�gB��B�3B�3B�-B� B}B|PB{B{Bz�B{dBy�Bu?Bs�Br�BsMBq�Bn}BkQBm�Bn}BjeBj�Bg�BgBbhB^�B^OB\BZkBY1BVBT�BQ�BRTBO�BL�BG�BHKBEBBAB@�B:�B9�B:�B:�B6`B7B5?B5�B4�B3�B0B.}B-]B,B*B*B($B'8B&�B$�B!|B�B#B�B_B9B2BgB�BaB�BTBbB�BBdB"BVB~BjB�BB�B�B�BbB�B.BvBHBB B B:B�B�B�B2BMBBMB�B�B#BxB B �B"NB"4B"�B#nB#TB$�B%�B&�B'�B*B+�B-�B-�B.�B0�B1�B2�B3�B3�B3�B5B5?B8�B=B>�B@BA;BCGBE�BF�BG_BL~BOvBOBBO\BO�BR�BT�BU�BZ�Bc BhXBi_Bi*Bn�Bp�Bq[Bt�B{dBHBcBHBHB~�B��B�tB��B��B��B�tB�mB�tB�fB��B�~B��B��B�B�'B��B�FB�B�RB�>B�_B��B��B��B��B��B��B�zB�lB� B�3B��B��B��B��B�:BՁB�7B�]B�~B��B�B��B��B��B��B��B��B��B�B�B�B�!B�B�B�ZB�LB�RB�^B��B�}B	;B	GB	_B	�B	~B	�B	vB	�B	�B	�B	B	!bB	%FB	%,B	&LB	&fB	'�B	'�B	'RB	(XB	(sB	)yB	)DB	*�B	,=B	,WB	-wB	.�B	/B	*�B	.�B	=�B	D�B	IB	OBB	MB	MB	L0B	LB	KB	J�B	K�B	Q�B	ZkB	^�B	`�B	a|B	b�B	f�B	h�B	i�B	j�B	o�B	o�B	p�B	r�B	vB	y	B	z*B	zB	{0B	|6B	}<B	~BB	�4B	�OB	�iB	�aB	�gB	�SB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	��B	��B	�B	��B	�B	� B	�:B	�LB	�DB	�6B	�6B	�OB	��B	�TB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�=B	�B	��B	�B	��B	�"B	��B	�B	�@B	�2B	�mB	�?B	�yB	ؓB	یB	�xB	ݘB	ޞB	ߤB	߾B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�"B	�B	��B	�B	��B	��B	��B	�B	�B	��B	�%B	�B	�B	�B	�8B	�$B	�	B	�*B	�^B	�6B	�<B	�VB	�(B	�]B	�cB	�cB
oB
�B
�B
�B
�B
YB
YB
YB
YB
YB
YB
YB
tB
tB
EB
zB
zB
�B
�B
�B
�B
	lB
	RB
	�B

�B

rB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 'B
"hB
#�B
!HB
�B
�B
�B
�B
�B
 �B
 �B
$&B
&LB
'RB
&B
($B
(>B
(XB
)*B
)B
)DB
*0B
+6B
+6B
+B
,WB
,WB
,WB
,qB
-wB
.}B
.}B
/iB
/iB
/iB
/iB
0UB
0oB
0UB
1vB
1vB
1�B
1vB
1vB
1[B
1�B
2|B
2|B
3hB
4�B
4nB
4nB
4�B
5�B
6�B
6�B
7�B
7�B
7�B
8�B
8�B
8lB
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
GB
HB
G�B
G�B
G�B
IB
I�B
I�B
J	B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
LB
LB
L0B
MB
MB
MB
MB
MB
NB
NB
N"B
N<B
N"B
N"B
N"B
NB
N"B
OB
O(B
OB
P.B
PB
P.B
PB
PB
O�B
PB
QB
QB
Q4B
Q4B
Q4B
QNB
R B
RB
R B
R B
SB
S&B
S&B
S&B
S@B
S[B
S@B
S@B
S@B
T,B
TFB
TaB
UB
U2B
U2B
UMB
UMB
U2B
UB
U2B
U2B
UMB
UMB
VSB
VSB
V9B
V�B
XEB
X_B
X_B
X_B
X_B
X_B
X_B
YeB
YeB
Y1B
YKB
YKB
ZkB
ZkB
ZkB
ZkB
ZkB
ZkB
ZkB
[WB
[qB
[qB
\xB
\xB
\]B
\xB
\xB
]dB
]dB
]IB
]dB
]dB
]dB
]dB
^jB
^OB
^jB
^OB
^�B
^OB
^jB
^jB
^�B
^�B
_pB
_pB
_�B
_�B
`vB
`vB
`vB
`vB
`vB
`�B
a�B
abB
a|B
abB
abB
a|B
a�B
a|B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
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
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�^<#�
<#�
<#�
<#�
<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.33(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610010033032016100100330320161001003303201806221214322018062212143220180622121432201804050407152018040504071520180405040715  JA  ARFMdecpA19c                                                                20160927093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160927003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160927003540  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160927003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160927003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160927003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160927003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160927003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160927003542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160927003542                      G�O�G�O�G�O�                JA  ARUP                                                                        20160927012202                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160927153558  CV  JULD            G�O�G�O�F�o�                JM  ARGQJMQC2.0                                                                 20160927153558  CV  JULD_LOCATION   G�O�G�O�F�o�                JM  ARGQJMQC2.0                                                                 20160927153558  CV  LONGITUDE       G�O�G�O��#��                JM  ARCAJMQC2.0                                                                 20160930153303  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160930153303  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190715  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031432  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111518                      G�O�G�O�G�O�                