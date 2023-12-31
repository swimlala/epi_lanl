CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-10T06:39:04Z creation;2020-01-10T06:39:09Z conversion to V3.1;2023-06-29T05:50:17Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Id   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200110063904  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_202                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @����? 1   @����Ԁ@7�z�G��b���IQ�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A���A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�3D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�p�A
�RA*�RAJ�RAj�RA�\)A�\)A�(�A�\)A�\)A�\)A�(�A�(�B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�BjG�Br�Bz�B�W
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
C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�H�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�H�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�D *�D ��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D	*�D	��D
*�D
�HD*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D *�D ��D!*�D!��D"*�D"��D#*�D#��D$*�D$��D%*�D%��D&*�D&��D'*�D'��D(*�D(��D)*�D)��D**�D*��D+*�D+��D,*�D,��D-*�D-��D.*�D.��D/*�D/��D0*�D0��D1*�D1��D2*�D2��D3*�D3��D4*�D4��D5*�D5��D6*�D6��D7*�D7��D8*�D8��D9*�D9��D:*�D:��D;*�D;��D<*�D<��D=*�D=��D>*�D>��D?*�D?��D@*�D@��DA*�DA��DB*�DB��DC*�DC��DD*�DD��DE*�DE��DF*�DF��DG*�DG��DH*�DH��DI*�DI��DJ*�DJ��DK*�DK��DL*�DL��DM*�DM��DN*�DN��DO*�DO��DP*�DP��DQ*�DQ��DR*�DR��DS*�DS��DT*�DT��DU*�DU��DV*�DV��DW*�DW��DX*�DX��DY*�DY��DZ*�DZ��D[*�D[��D\*�D\��D]*�D]��D^*�D^��D_*�D_��D`*�D`��Da*�Da��Db*�Db��Dc*�Dc��Dd*�Dd��De*�De��Df*�Df��Dg*�Dg��Dh*�Dh��Di*�Di��Dj*�Dj��Dk*�Dk��Dl*�Dl��Dm*�Dm��Dn*�Dn��Do*�Do��Dp*�Dp��Dq*�Dq��Dr*�Dr��Ds*�Ds��Dt*�Dt��Du*�Du��Dv*�Dv��Dw*�Dw��Dx*�Dx��Dy*�Dy��Dz*�Dz��D{*�D{��D|*�D|��D}*�D}��D~*�D~��D*�D��D�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD�ؤD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�=D�R=D��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD��D�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�=D�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqDqD��qD�qD�UqDÕqD��qD�qD�UqDĕqD��qD�qD�UqDŕqD��qD�qD�UqDƕqD��qD�qD�UqDǕqD��qD�qD�UqDȕqD��qD�qD�UqDɕqD��qD�qD�UqDʕqD��qD�qD�UqD˕qD��qD�qD�UqD̕qD��qD�qD�UqD͕qD��qD�qD�UqDΕqD��qD�qD�UqDϕqD��qD�qD�UqDЕqD��qD�qD�UqDѕqD��qD�qD�UqDҕqD��qD�qD�UqDӕqD��qD�qD�UqDԕqD��qD�qD�UqDՕqD��qD�qD�UqD֕qD��qD�qD�UqDוqD��qD�qD�UqDؕqD��qD�qD�UqDٕqD��qD�qD�UqDڕqD��qD�qD�UqDەqD��qD�qD�UqDܕqD��qD�qD�UqDݕqD��qD�qD�UqDޕqD��qD�qD�UqDߕqD��qD�qD�UqD��qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD��=D��qD�qD�UqD��qD��qD��D�eq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�&�A�&�A�(�A�(�A�+A�+A�/A�-A�(�A�-A�1'A�7LA�;dA�=qA�;dA�=qA�33A�5?A�;dA�C�A�C�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�I�A�G�A�K�A�M�A�O�A�K�A�K�A�O�A�S�A�S�A�VA�VA�O�A�O�A�M�A�G�A��A��yAżjAŕ�A�
=A��;AĸRA�;dAÇ+A�C�A�1A�I�A�jA�A�A���A�33A���A�r�A�Q�A�|�A��A��!A���A�oA�ZA���A��A���A�l�A��A�I�A���A�ĜA���A�(�A���A�l�A�A�jA�VA��`A��A���A���A�%A�^5A�1'A���A��A��/A�p�A�|�A�7LA���A�?}A��7A�l�A�?}A���A�S�A��DA���A�|�A�ĜA��FA��A��A�hsA�\)A�A~A�A{�wAxI�Av��Au��Ap�RAmVAi��Ag�7Af �Adr�Aa7LA\�9AXr�AV1AS��AQ�AN�AL��AK��AJA�AH�AG+AF=qAE��AE`BAD�`AD1ABQ�A@bA?%A=&�A<�A:�A:1'A9�;A9��A8�!A6�jA5G�A4r�A3O�A1\)A0�DA/G�A-��A-"�A+�A+33A*��A)�^A)��A)+A(�jA(Q�A&�9A%ƨA$�\A"ĜA!�-A �A ��A��A�PAn�A+A�wAVA�RAE�A�FA�!A�AdZA�A��AVA�hA33A�DA�-AVA5?A�AVA�A��A�A�AK�A
^5A	�
A	�^A��A�^AXAI�A7LA �At�A�9A`BA �A 9X@��#@�?}@���@�X@���@�1@@��T@��/@��
@�ff@�hs@�Q�@�b@睲@��@�D@�  @�33@���@�v�@���@�9@ߕ�@�-@��@���@�|�@�{@�Z@�K�@�o@�ȴ@և+@�t�@�;d@���@Չ7@�9X@�1'@�^5@���@��H@�$�@�hs@̬@��m@�n�@���@ɺ^@Ǯ@�^5@�J@�&�@�A�@�ƨ@�@�~�@��@��j@�1@�t�@���@�@�p�@���@�+@�ff@��-@�`B@��m@�+@�V@��@�ff@�ff@���@�v�@��@���@�5?@���@�V@��\@���@��@���@�v�@���@���@��@�l�@�C�@�$�@���@�~�@��@�@��#@���@��
@�v�@���@���@�  @�S�@���@�=q@���@�V@��D@�Q�@��@��P@�\)@�ff@��@�@�$�@���@��@��@��!@�M�@���@���@�|�@�ƨ@�A�@�X@��@���@���@��@���@��@�+@��!@�v�@���@��H@��+@���@��^@�X@�&�@��T@�G�@��u@� �@�  @��@�C�@�dZ@�K�@��@���@�=q@�^5@�M�@�$�@�5?@��#@���@�?}@���@�(�@�l�@��@�"�@���@���@�V@��@�bN@�E�@��@���@�ȴ@���@�/@�@��-@��@�Ĝ@��/@�/@�hs@��/@�(�@��@�bN@��u@��F@�t�@��\@��@�G�@���@�Q�@��@�`B@���@��@�A�@�bN@�\)@�+@�33@�dZ@�dZ@��\@���@���@�ȴ@��@�o@�+@�l�@�;d@�@�hs@�%@��`@��9@�|�@�o@���@��\@�~�@�ff@�@���@��@�hs@�/@��`@��/@���@��9@���@�r�@�(�@��m@��@�\)@�"�@�o@���@�ȴ@���@�ff@�$�@��@��-@��@�`B@�/@�Ĝ@��j@�j@��@���@�t�@�dZ@��@��@��R@�$�@��^@�x�@�O�@�?}@�7L@�%@��`@�z�@�bN@�A�@�@�@l�@
=@~ȴ@~ff@~V@~E�@~{@}�@}�@|�j@|�@|�@|��@|Z@|�@{��@{��@{t�@{S�@{"�@z��@zJ@y��@y7L@y%@x��@x�@xQ�@w��@vȴ@v5?@u@u��@t��@t��@t�D@s�m@s��@sS�@r�H@rn�@rJ@q�#@qhs@pĜ@p�@p �@o�w@o�@nȴ@nE�@n{@m@m�h@m�@m�@lj@l9X@l�@kƨ@k�@k33@j�!@jn�@j-@jJ@i�^@i�7@i�7@ix�@ihs@i7L@h�9@g�@g��@g\)@g�@f5?@ep�@d��@dj@c�m@c��@c@b��@b~�@bn�@bM�@b�@bJ@bJ@a�^@aX@a%@`r�@`b@_�@_\)@_;d@_
=@^�R@^V@]�T@]`B@\��@\��@\�D@\I�@[�
@[t�@[33@Z�@Z��@Z��@Z^5@Y��@Y�^@Y�^@Yhs@YG�@X�`@Xr�@X1'@X  @W�;@W�P@V�y@Vff@V@U@Up�@UO�@U�@T�@T��@T��@T��@TI�@T(�@T1@T1@S�
@S��@SC�@R�@R�\@R^5@Q�@Q��@Q�^@Q�7@Q7L@P�`@PA�@O�w@O�@N�y@N�+@N5?@N@M��@M�@L�@L�D@Lz�@K��@K�
@K��@KS�@J��@J��@J��@J~�@J^5@J=q@I��@Ihs@I&�@H��@Hr�@G�;@G|�@G�@F�R@Fv�@FE�@F$�@E�T@E�-@E�@EO�@D�j@D9X@D1@D1@C�m@C�@C"�@B��@B=q@B�@B�@A��@A��@A7L@@�`@@�u@@1'@@b@@  @?��@?��@?l�@?;d@?�@>�y@>�R@>v�@>5?@>{@>@=@=��@=/@<�j@<�D@<z�@<I�@<�@;�
@;ƨ@;ƨ@;�F@;S�@;"�@;o@:�H@:�\@:n�@:M�@:-@9��@9��@9%@8r�@8  @7�w@7l�@7;d@7
=@6�@6��@6�+@6v�@6E�@6{@5��@5�h@5O�@5V@4��@4�@4�@4�@4z�@49X@4(�@41@3�m@3ƨ@3��@3�@3S�@3"�@3o@2�@2��@2^5@1��@1�#@1hs@1X@1G�@1%@0Ĝ@0r�@0 �@/��@/�@/�P@/K�@/+@/
=@/
=@.ȴ@.�+@.V@.{@-@-�h@-�@-`B@-?}@-V@,�@,�/@,�@,Z@,(�@+��@+33@+@*��@*�\@*=q@)�@)��@)��@)7L@(��@(Ĝ@(��@(�@( �@'��@'�P@'l�@';d@&ȴ@&�+@&V@%�@%�T@%��@%��@%`B@%?}@%�@$�/@$��@$I�@$1@#�
@#t�@#"�@#o@#@"�H@"~�@"^5@"=q@!��@!�#@!��@!hs@!G�@!%@ r�@ A�@  �@   @�;@��@|�@\)@
=@�R@v�@E�@{@�T@�-@p�@V@�/@�D@(�@�@�m@ƨ@S�@C�@"�@�@��@��@^5@M�@�@x�@G�@��@bN@1'@1'@�@��@��@�w@�P@;d@
=@�y@ȴ@ff@E�@{@5?@5?@{@�@@��@�h@�@?}@/@�@��@��@�j@�D@Z@9X@�@�F@dZ@t�@C�@"�@�@�H@�\@^5@=q@��@��@��@x�@X@G�@X@X@&�@��@��@��@Ĝ@�u@�u@r�@Q�@ �@  @��@�@\)@;d@��@ȴ@��@E�@v�@E�@�@@��@@�-@�h@p�@V@��@�@��@��@�D@j@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�&�A�&�A�(�A�(�A�+A�+A�/A�-A�(�A�-A�1'A�7LA�;dA�=qA�;dA�=qA�33A�5?A�;dA�C�A�C�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�I�A�K�A�I�A�G�A�K�A�M�A�O�A�K�A�K�A�O�A�S�A�S�A�VA�VA�O�A�O�A�M�A�G�A��A��yAżjAŕ�A�
=A��;AĸRA�;dAÇ+A�C�A�1A�I�A�jA�A�A���A�33A���A�r�A�Q�A�|�A��A��!A���A�oA�ZA���A��A���A�l�A��A�I�A���A�ĜA���A�(�A���A�l�A�A�jA�VA��`A��A���A���A�%A�^5A�1'A���A��A��/A�p�A�|�A�7LA���A�?}A��7A�l�A�?}A���A�S�A��DA���A�|�A�ĜA��FA��A��A�hsA�\)A�A~A�A{�wAxI�Av��Au��Ap�RAmVAi��Ag�7Af �Adr�Aa7LA\�9AXr�AV1AS��AQ�AN�AL��AK��AJA�AH�AG+AF=qAE��AE`BAD�`AD1ABQ�A@bA?%A=&�A<�A:�A:1'A9�;A9��A8�!A6�jA5G�A4r�A3O�A1\)A0�DA/G�A-��A-"�A+�A+33A*��A)�^A)��A)+A(�jA(Q�A&�9A%ƨA$�\A"ĜA!�-A �A ��A��A�PAn�A+A�wAVA�RAE�A�FA�!A�AdZA�A��AVA�hA33A�DA�-AVA5?A�AVA�A��A�A�AK�A
^5A	�
A	�^A��A�^AXAI�A7LA �At�A�9A`BA �A 9X@��#@�?}@���@�X@���@�1@@��T@��/@��
@�ff@�hs@�Q�@�b@睲@��@�D@�  @�33@���@�v�@���@�9@ߕ�@�-@��@���@�|�@�{@�Z@�K�@�o@�ȴ@և+@�t�@�;d@���@Չ7@�9X@�1'@�^5@���@��H@�$�@�hs@̬@��m@�n�@���@ɺ^@Ǯ@�^5@�J@�&�@�A�@�ƨ@�@�~�@��@��j@�1@�t�@���@�@�p�@���@�+@�ff@��-@�`B@��m@�+@�V@��@�ff@�ff@���@�v�@��@���@�5?@���@�V@��\@���@��@���@�v�@���@���@��@�l�@�C�@�$�@���@�~�@��@�@��#@���@��
@�v�@���@���@�  @�S�@���@�=q@���@�V@��D@�Q�@��@��P@�\)@�ff@��@�@�$�@���@��@��@��!@�M�@���@���@�|�@�ƨ@�A�@�X@��@���@���@��@���@��@�+@��!@�v�@���@��H@��+@���@��^@�X@�&�@��T@�G�@��u@� �@�  @��@�C�@�dZ@�K�@��@���@�=q@�^5@�M�@�$�@�5?@��#@���@�?}@���@�(�@�l�@��@�"�@���@���@�V@��@�bN@�E�@��@���@�ȴ@���@�/@�@��-@��@�Ĝ@��/@�/@�hs@��/@�(�@��@�bN@��u@��F@�t�@��\@��@�G�@���@�Q�@��@�`B@���@��@�A�@�bN@�\)@�+@�33@�dZ@�dZ@��\@���@���@�ȴ@��@�o@�+@�l�@�;d@�@�hs@�%@��`@��9@�|�@�o@���@��\@�~�@�ff@�@���@��@�hs@�/@��`@��/@���@��9@���@�r�@�(�@��m@��@�\)@�"�@�o@���@�ȴ@���@�ff@�$�@��@��-@��@�`B@�/@�Ĝ@��j@�j@��@���@�t�@�dZ@��@��@��R@�$�@��^@�x�@�O�@�?}@�7L@�%@��`@�z�@�bN@�A�@�@�@l�@
=@~ȴ@~ff@~V@~E�@~{@}�@}�@|�j@|�@|�@|��@|Z@|�@{��@{��@{t�@{S�@{"�@z��@zJ@y��@y7L@y%@x��@x�@xQ�@w��@vȴ@v5?@u@u��@t��@t��@t�D@s�m@s��@sS�@r�H@rn�@rJ@q�#@qhs@pĜ@p�@p �@o�w@o�@nȴ@nE�@n{@m@m�h@m�@m�@lj@l9X@l�@kƨ@k�@k33@j�!@jn�@j-@jJ@i�^@i�7@i�7@ix�@ihs@i7L@h�9@g�@g��@g\)@g�@f5?@ep�@d��@dj@c�m@c��@c@b��@b~�@bn�@bM�@b�@bJ@bJ@a�^@aX@a%@`r�@`b@_�@_\)@_;d@_
=@^�R@^V@]�T@]`B@\��@\��@\�D@\I�@[�
@[t�@[33@Z�@Z��@Z��@Z^5@Y��@Y�^@Y�^@Yhs@YG�@X�`@Xr�@X1'@X  @W�;@W�P@V�y@Vff@V@U@Up�@UO�@U�@T�@T��@T��@T��@TI�@T(�@T1@T1@S�
@S��@SC�@R�@R�\@R^5@Q�@Q��@Q�^@Q�7@Q7L@P�`@PA�@O�w@O�@N�y@N�+@N5?@N@M��@M�@L�@L�D@Lz�@K��@K�
@K��@KS�@J��@J��@J��@J~�@J^5@J=q@I��@Ihs@I&�@H��@Hr�@G�;@G|�@G�@F�R@Fv�@FE�@F$�@E�T@E�-@E�@EO�@D�j@D9X@D1@D1@C�m@C�@C"�@B��@B=q@B�@B�@A��@A��@A7L@@�`@@�u@@1'@@b@@  @?��@?��@?l�@?;d@?�@>�y@>�R@>v�@>5?@>{@>@=@=��@=/@<�j@<�D@<z�@<I�@<�@;�
@;ƨ@;ƨ@;�F@;S�@;"�@;o@:�H@:�\@:n�@:M�@:-@9��@9��@9%@8r�@8  @7�w@7l�@7;d@7
=@6�@6��@6�+@6v�@6E�@6{@5��@5�h@5O�@5V@4��@4�@4�@4�@4z�@49X@4(�@41@3�m@3ƨ@3��@3�@3S�@3"�@3o@2�@2��@2^5@1��@1�#@1hs@1X@1G�@1%@0Ĝ@0r�@0 �@/��@/�@/�P@/K�@/+@/
=@/
=@.ȴ@.�+@.V@.{@-@-�h@-�@-`B@-?}@-V@,�@,�/@,�@,Z@,(�@+��@+33@+@*��@*�\@*=q@)�@)��@)��@)7L@(��@(Ĝ@(��@(�@( �@'��@'�P@'l�@';d@&ȴ@&�+@&V@%�@%�T@%��@%��@%`B@%?}@%�@$�/@$��@$I�@$1@#�
@#t�@#"�@#o@#@"�H@"~�@"^5@"=q@!��@!�#@!��@!hs@!G�@!%@ r�@ A�@  �@   @�;@��@|�@\)@
=@�R@v�@E�@{@�T@�-@p�@V@�/@�D@(�@�@�m@ƨ@S�@C�@"�@�@��@��@^5@M�@�@x�@G�@��@bN@1'@1'@�@��@��@�w@�P@;d@
=@�y@ȴ@ff@E�@{@5?@5?@{@�@@��@�h@�@?}@/@�@��@��@�j@�D@Z@9X@�@�F@dZ@t�@C�@"�@�@�H@�\@^5@=q@��@��@��@x�@X@G�@X@X@&�@��@��@��@Ĝ@�u@�u@r�@Q�@ �@  @��@�@\)@;d@��@ȴ@��@E�@v�@E�@�@@��@@�-@�h@p�@V@��@�@��@��@�D@j@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�HB
�B
�B
��BB	7BPB�B�B:^Bm�B��B��B�B�ZB�BB�B�B&�B/B6FB8RB<jB<jBG�BN�BL�BK�BC�B:^B0!B(�B�BoB�B�BBBoB�B�B�B1B��B��B�B�fB�)B�B�BȴB�^B��B��Bt�Be`BR�B33B�B
��B
�;B
B
�?B
��B
�B
gmB
C�B
/B
'�B
�B
oB	��B	�B	�sB	ŢB	��B	�hB	�B	v�B	k�B	YB	<jB	"�B	oB	B��B�B�5B�
B��BȴBÖB�}B�dB�^B�LB�-B�B��B��B��B��B�{B�oB�uB�uB�{B�oB�7B�JB�7B�+B�B|�Bw�Bt�Bt�Bp�Bq�Bt�Bt�Bz�B� B� B|�Bx�Bu�Bl�BgmBdZBr�Bs�Bp�Bw�Bv�Bu�Bt�Bt�Bt�Bs�Bs�Bs�Br�Br�Br�Br�Br�Bs�Bq�Bn�Bo�Bn�Bk�Bq�Bu�Bt�Bt�Br�Bq�Bn�Bm�Bq�Br�Bm�Bk�Be`Be`BffBaHB]/BVBP�BK�BH�B?}B8RB6FB.B/B.B/B0!B1'B49B33B33B2-B33B1'B/B2-B6FB9XB:^B:^B=qBD�BF�BF�BE�BF�BE�BH�BI�BK�BM�BP�B^5B`BBbNBbNB`BBe`BgmBcTBcTBk�Bp�Bq�Bp�Bq�Bx�Bw�Bq�Bm�Bl�Bl�Bn�Bn�Bn�Bp�Bq�Bt�Bs�Bw�B{�Bz�Bz�B�B{�By�B|�B}�B�B�%B�uB��B�B�9B�LB�RB�LB�-B�B�B�9B�XB��BÖBƨB��B��B��B��B��B��B��BɺBǮB��B��B��B��B��B��B�B��B��B�B�B�;B�ZB�mB�B�B�B��B��B��B��B��B	B		7B	VB	hB	{B	�B	{B	�B	'�B	-B	1'B	:^B	:^B	:^B	@�B	N�B	K�B	N�B	[#B	`BB	`BB	bNB	dZB	dZB	cTB	ffB	gmB	gmB	m�B	n�B	o�B	q�B	s�B	v�B	w�B	x�B	z�B	z�B	z�B	|�B	� B	�B	�B	�%B	�=B	�=B	�JB	�VB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	�hB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�FB	�RB	�XB	�^B	�^B	�XB	�^B	�XB	�XB	�RB	�'B	�B	�B	�B	�-B	�!B	�'B	�-B	�9B	�FB	�9B	�FB	�wB	�jB	�}B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�HB	�NB	�NB	�TB	�ZB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
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
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
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
Q�B
Q�B
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
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
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
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
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
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
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
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
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
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ѷB
ҽB
ҽB
��B
ѷB
ѷB
ѷB
ҽB
ҽB
ҽB
��B
ҽB
ҽB
ҽB
ҽB
ҽB
ҽB
ҽB
ҽB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�MB
�|B
�B
��B
��BSB	�B�B�B"�BAUBtTB�B�{B�1B�FB�B�B1B \B)�B1�B88B:B?�B>BI�BPHBO�BM�BE�B;�B1�B*�B�B&B�BBB�B[B �B vB�B	�B �B�LB��B��B��B�1B�+B�xB��B��B��BwfBh
BU�B7B!�B
�cB
�4B
ĶB
�B
�nB
��B
k�B
E�B
0oB
)�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	yXB	o�B	^OB	@�B	%�B	2B	�B��B��BߤBخB��B�	BāB��B��B��B��B�9B�UB�FB��B��B��B�B��B��B��B��B��B�rB��B�)B�1B��B~BBx�Bu�ButBqABraBt�Bu%B{dB��B��B~(Bz^Bw�Bm�Bh$BeBu%Bt�BrBy$BxBv`BuBu?ButBt�BtnBtBr�Br�Bs3BshBsBtnBr�BoiBp�BpBk�BrBvBu�Bu�BshBr|BoBm�Br�Bs�Bn/Bl�Bf�Bf�BgBbNB^jBV�BRBM�BJ�B@�B9�B8B/5B/�B.}B/�B0�B1�B4�B3�B3MB2|B4B1�B/OB2|B6FB9�B:�B:�B>BE9BF�BF�BFYBG_BFYBIBI�BK�BM�BPbB^B`vBb�Bb�B`vBffBhsBc�Bc�Bk�Bp�BrBqBq�ByrBx�Br-Bm�Bl�Bl�Bn�Bn�Bn�Bp�BrGBt�Bs�BxB|B{Bz�B�GB|6Bz*B}B~wB��B�B��B��B��B��B�LB��B�8B��B��B��B��B�	B��B�aB�?B� BՁB�aB��B��BԯB�B�=BǮBʌB��BՁB�[BӏB�aB�mB�,B�@B�1B�B�pB�ZB�B�B�B�B��B�B��B��B��B	oB	�B	B	NB	{B	SB	�B	�B	'RB	,=B	0;B	:B	9�B	9XB	@B	O�B	KB	M�B	Z�B	`B	_�B	a�B	d@B	d�B	c B	fLB	g8B	f�B	m�B	n�B	o�B	qvB	s�B	v�B	wfB	x�B	z�B	z�B	z�B	|�B	�B	��B	��B	�B	�#B	�=B	�dB	�pB	��B	�"B	�4B	�gB	�eB	��B	��B	�]B	�_B	�4B	��B	��B	��B	�mB	�B	�~B	��B	�pB	�|B	�tB	��B	�/B	�'B	��B	��B	��B	��B	�XB	��B	�xB	�rB	�xB	�rB	��B	�$B	�'B	� B	�B	��B	�GB	�B	��B	��B	�B	�FB	��B	��B	�wB	�B	�.B	�MB	�KB	��B	�"B	��B	ˬB	˒B	��B	�(B	͹B	��B	ΊB	ΊB	ΥB	οB	οB	ΥB	ΥB	οB	ΥB	ΥB	ΥB	ϑB	ϑB	��B	��B	ϫB	��B	ѝB	��B	ҽB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�B	�BB	�-B	�4B	�B	�B	�@B	�LB	�fB	�B	�_B	�kB	�=B	�=B	�=B	�qB	�wB	�wB	�IB	�iB	�B	�UB	�UB	�B	�|B	�aB	�MB	�B	�B	�B	�tB	�zB	�zB	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
B
�B
	B
	B

	B
	�B
	�B

	B

	B

	B
B
B
B
B
B
B
"B
B
B
(B
B
B
�B
.B
B
.B
HB
bB
4B
:B
TB
TB
uB
@B
[B
aB
FB
aB
2B
9B
9B
9B
9B
9B
9B
SB
SB
YB
sB
sB
_B
EB
KB
eB
eB
�B
kB
�B
qB
WB
WB
xB
�B
xB
~B
dB
�B
�B
�B
�B
jB
pB
pB
�B
�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
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
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
/ B
.�B
0B
/�B
0B
0�B
0�B
1B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
3B
4B
5B
4�B
5B
5%B
5B
5B
6B
7B
6�B
7B
7B
72B
7B
6�B
8B
8B
9	B
9$B
9$B
9$B
:*B
:*B
:*B
:*B
:*B
:*B
;0B
;B
;0B
;B
;JB
<B
<B
<B
<6B
=<B
=<B
=B
="B
=<B
>BB
>BB
>BB
>(B
?HB
@4B
@OB
@OB
@OB
@iB
AUB
B[B
AUB
A;B
AUB
A;B
A;B
BAB
BAB
BAB
BAB
B[B
B[B
B[B
B[B
CaB
CaB
CaB
CGB
D3B
DMB
ESB
ESB
E9B
EmB
EmB
ESB
ESB
FYB
FtB
FtB
FtB
GzB
G_B
GzB
HfB
HfB
GzB
G_B
G_B
GzB
HfB
HfB
H�B
I�B
IlB
I�B
I�B
I�B
I�B
JrB
J�B
J�B
K�B
K�B
K�B
KxB
KxB
K�B
K�B
KxB
L�B
L~B
L�B
L�B
L~B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
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
Q�B
Q�B
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
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
^B
^B
^B
^B
^B
^B
^�B
^�B
^�B
_B
^�B
_�B
`B
`B
aB
aB
`�B
aB
aB
aB
a�B
a�B
c B
cB
c B
c B
cB
c B
d&B
dB
dB
d&B
dB
dB
d&B
e,B
e,B
eB
e,B
e,B
d�B
eB
fB
f2B
f2B
fB
f2B
f2B
fB
f2B
f2B
gB
gB
gB
h$B
h
B
h$B
iDB
i*B
iDB
iB
iDB
j0B
jKB
j0B
jKB
jKB
jKB
jKB
j0B
jKB
j0B
jKB
jKB
kQB
k6B
l=B
l=B
l=B
lWB
lWB
mCB
mCB
m]B
m]B
m]B
mCB
m)B
mCB
m]B
nIB
ncB
nIG�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.67(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001150034052020011500340520200115003405202306231720042023062317200420230623172004202001160023132020011600231320200116002313  JA  ARFMdecpA19c                                                                20200110153830  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200110063904  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200110063906  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200110063907  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200110063908  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200110063908  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200110063908  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200110063908  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200110063909  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200110063909                      G�O�G�O�G�O�                JA  ARUP                                                                        20200110065431                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200110153855  CV  JULD            G�O�G�O�F��'                JM  ARSQJMQC2.0                                                                 20200114000000  CF  PSAL_ADJUSTED_QCD�P D�P G�O�                JM  ARCAJMQC2.0                                                                 20200114153405  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200114153405  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200115152313  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082004  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                