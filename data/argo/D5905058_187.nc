CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-07T06:37:46Z creation;2019-11-07T06:37:50Z conversion to V3.1;2023-06-29T05:50:45Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191107063746  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_187                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @���-� 1   @���W @7:��S&�b�Q��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @dz�@�p�@أ�A
�RA*�RAJ�RAj�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B
�B�B�B"�B*�B2�B:�BC{BJ�BR�BZ�Bb�Bj�Br�Bz�B�W
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
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�H�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�b�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�H�C�U�C�U�C�U�C�b�C�b�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�D *�D ��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D	*�D	��D
*�D
��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D${D��D *�D �{D!*�D!��D"*�D"��D#*�D#��D$*�D$��D%*�D%��D&*�D&��D'*�D'��D(*�D(��D)*�D)��D**�D*��D+*�D+��D,1HD,��D-*�D-��D.*�D.��D/*�D/��D0*�D0��D1*�D1��D2*�D2��D3*�D3��D4*�D4��D5*�D5��D6*�D6��D7*�D7��D8*�D8��D9*�D9��D:*�D:��D;*�D;��D<*�D<��D=*�D=��D>*�D>��D?*�D?��D@*�D@��DA*�DA��DB*�DB��DC*�DC��DD*�DD��DE*�DE��DF*�DF��DG*�DG��DH*�DH��DI*�DI��DJ${DJ��DK*�DK��DL*�DL��DM*�DM��DN*�DN��DO*�DO��DP*�DP��DQ*�DQ��DR*�DR��DS*�DS��DT*�DT��DU*�DU��DV*�DV��DW*�DW��DX*�DX��DY*�DY��DZ*�DZ��D[*�D[��D\*�D\��D]*�D]��D^*�D^��D_*�D_��D`*�D`��Da*�Da��Db*�Db��Dc*�Dc��Dd*�Dd��De*�De��Df*�Df��Dg*�Dg��Dh*�Dh�{Di*�Di��Dj*�Dj��Dk*�Dk��Dl*�Dl��Dm*�Dm��Dn*�Dn��Do*�Do��Dp*�Dp��Dq*�Dq��Dr*�Dr��Ds*�Ds��Dt*�Dt��Du*�Du��Dv*�Dv��Dw*�Dw��Dx*�Dx��Dy*�Dy��Dz*�Dz��D{*�D{��D|*�D|��D}1HD}��D~*�D~��D*�D��D�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqDqD��qD�qD�UqDÕqD��qD��D�UqDĕqD��qD�qD�UqDŕqD��qD�qD�UqDƕqD��qD�qD�UqDǕqD��qD�qD�UqDȕqD��qD�qD�UqDɕqD��qD�qD�UqDʕqD��qD�qD�UqD˕qD��qD�qD�UqD̕qD��qD�qD�UqD͒=D��qD�qD�UqDΕqD��qD�qD�UqDϕqD��qD�qD�UqDЕqD��qD�qD�UqDѕqD��qD�qD�UqDҕqD��qD�qD�UqDӕqD��qD�qD�UqDԕqD��qD�qD�UqDՕqD��qD�qD�UqD֕qD��qD�qD�UqDוqD��qD�qD�UqDؕqD��qD�qD�UqDٕqD��qD�qD�UqDڕqD��qD�qD�UqDەqD��qD�qD�UqDܕqD��qD�=D�UqDݕqD��qD�qD�UqDޕqD��qD�qD�UqDߕqD��qD�qD�UqD��qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD���D�ؤD��D�;�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴAә�A�^5A�VA�K�A�;dA�&�A��A��A�{A��A�bA�1A�1A�  A�A�A�  A���A���A��
A���AҶFAҡ�AҁA�t�A�dZA�`BA�^5A�?}Aѡ�AϸRA�
=A�l�AǅA�x�A�?}A�1A�oA��-A�ȴA�O�A��A��;A�(�A�O�A�p�A���A�A�G�A�A�A�
=A��RA��A���A��A�S�A���A��A���A��mA�dZA��;A��+A��yA�VA�A�A�A��!A��`A�z�A��A�`BA�oA��A��mA�z�A��A�1A��A�z�A�
=A��-A�
=A�dZA�1'A���A�n�A�x�A��7A��uA���A��-A��`A��uA��!A���A�+A��DA�A�t�A�S�A��A��;A���A��AA|VAy|�Au��Ar�`Ap�\AlM�Aj��Aip�Ag�TAg��Af5?Adr�Ab�A`�A^r�A[�^AZ  AWƨAUt�AT$�AR�AP�AN�9AM|�AL��ALffAK
=AJJAHZAFȴAD�!ACoA@��A@-A>^5A<�uA;`BA:5?A8�/A7A6jA5VA3�;A3�A2jA0�A.��A-33A,ffA+�A+�PA+A*ĜA*A)�A(bNA&�!A&-A%��A%��A#�-A#C�A"bNA!"�A �\A�PA��AC�A��AA�\A�-Az�AO�A��A��A  A��Ar�A5?A�#At�A%A�+A�!A
�AdZAdZA
��A
A	C�A5?A�FAl�A/A9XA|�A
=A�DAhsA�A ��A ~�@�;d@��u@�Z@�33@�7L@�z�@�l�@��@��@�S�@@�+@�7@��@��@�|�@�n�@�bN@旍@�O�@�(�@�w@◍@��@�|�@�@�t�@��T@��@�&�@�`B@�x�@݉7@�J@��H@�C�@ߥ�@ߥ�@۾w@�Q�@�S�@թ�@���@Լj@ӶF@ӍP@�9X@�l�@�l�@�@ύP@�%@�C�@���@�X@�K�@�/@���@�O�@��y@�=q@ɡ�@ȣ�@�9X@Ǖ�@� �@���@�dZ@��@���@�G�@�l�@�K�@°!@�~�@+@���@���@��j@��D@���@���@�S�@��@��+@�$�@��#@��-@�-@��@��;@�ff@���@�hs@�`B@���@��j@�5?@��@��w@�dZ@�;d@�@��H@���@�-@��@�%@�Ĝ@���@�bN@�r�@�9X@��m@�1@���@�S�@��@�33@���@�V@���@��#@��-@��@��@��@�l�@�K�@���@�M�@��/@��!@���@�p�@�?}@�V@��`@�(�@�33@��y@���@���@��\@�~�@�^5@�-@�J@��-@��@�Ĝ@�Q�@�1'@�t�@�^5@�X@�j@��P@�V@�@��@�j@��@��;@��w@���@�V@�j@��F@�S�@��D@�|�@���@��-@���@�x�@�p�@�O�@�7L@��@�j@�1'@�A�@�b@���@���@�E�@��@��`@�r�@���@�l�@�33@�
=@�o@�o@���@���@�$�@��@���@���@��7@�7L@��@��/@���@�j@�b@��@�;d@�"�@�
=@��@�ȴ@��!@���@�~�@�^5@�V@�E�@�{@���@���@��h@�`B@��@���@��D@�r�@�j@�I�@�1'@�(�@�  @��m@��
@���@�ƨ@��w@��@�C�@��@��H@��@��@��!@��\@��+@�V@�$�@���@���@�p�@�G�@�V@�%@��@���@��j@��@��9@��9@��9@�j@�(�@��@��F@�"�@���@���@���@�v�@�V@��@���@��-@�X@�V@���@��@��u@�bN@�A�@�  @�ƨ@���@��@�l�@�+@�
=@���@�~�@�J@�J@�J@��@�p�@�O�@��@���@���@�r�@�A�@�b@~�y@~v�@~5?@}�@}�h@}V@|�@|z�@|I�@|1@{S�@z�@z�!@z��@z~�@zn�@zn�@zM�@y��@y&�@xĜ@x �@w�@w\)@v�R@vE�@u�@u`B@u�@t�@t�j@tZ@s��@s�F@sdZ@s33@r��@r^5@r=q@rJ@q&�@pA�@pb@o�;@o|�@n�y@n�R@nV@n$�@m��@m`B@l�/@l�D@lZ@l(�@k��@k�
@kƨ@k�F@k33@j��@jn�@jM�@jJ@i�^@ix�@iG�@hĜ@h�u@h�@h�@hA�@g�@g�w@gl�@g�@g
=@f�y@f�@f��@f{@e�@e@ep�@e/@d��@d�/@d�@dz�@d9X@c��@c��@c�@cdZ@c33@b�@b~�@b^5@b�@a�7@aG�@a&�@`��@`�u@`A�@_�w@_l�@_+@^�R@^{@]�@]/@]V@\��@\Z@[�
@[t�@[S�@[33@Z�@Z~�@Z-@ZJ@Y�^@YX@X��@X�9@X�u@XQ�@W��@W|�@WK�@W;d@V��@V�R@V�@Vȴ@V�+@V5?@T�@S�m@S�F@S��@R�@R�!@R^5@R�@Qx�@P��@PbN@PQ�@P1'@P  @O|�@O�@Nȴ@N��@Nv�@NV@N{@M�T@M�-@M`B@L�@L�D@L9X@Kƨ@Kt�@K@J=q@I�#@Ix�@H��@H�@HQ�@G�@G�@G�P@G|�@Gl�@G;d@Fȴ@Fv�@FE�@E@Ep�@E?}@D�j@D�@C��@C�
@C��@CS�@C@B��@B~�@B�@B-@B-@B-@B�@A�#@A�^@A��@AX@@��@@��@@bN@@b@?|�@?;d@?�@?�@>�@>ff@=��@=�h@=`B@=?}@=/@=V@<�/@<j@<1@;t�@;"�@;"�@;o@:�@:��@9��@9�^@9��@9�@8�u@81'@7��@7l�@7�@6��@6�@6��@6E�@6{@5�T@5@5�h@5�@4�j@4j@4I�@4(�@41@3�
@3t�@3S�@3o@2��@2�!@2^5@1��@1��@1G�@17L@17L@1&�@0��@0�9@0�@0bN@0 �@/�w@/��@/l�@/K�@/;d@/�@.�y@.�R@.��@.v�@.V@.5?@.$�@.@-p�@,��@,��@,��@,�/@,��@,1@+�F@+��@+�@+dZ@*�@*��@*�\@*^5@)��@)�^@)X@)%@(��@(A�@(b@'�w@'l�@'K�@'+@&�@&ff@&E�@&$�@%�@%?}@$�@$��@$��@$z�@$9X@$�@#��@#�
@#�@#t�@#C�@#@"�!@"M�@"-@!�@!��@!��@!�7@!x�@!&�@ ��@ �9@ �u@ r�@  �@  �@ b@ b@��@\)@+@
=@��@��@v�@E�@$�@@�h@`B@?}@�@�@�j@��@�D@z�@I�@(�@�
@�F@��@t�@"�@o@�@��@��@n�@M�@-@��@��@��@��@x�@G�@��@�u@r�@bN@1'@  @�;@�w@��@l�@K�@��@�y@�+@v�@5?@�@�T@��@��@`B@/@V@�@�D@I�@(�@�m@�F@�@C�@�@�H@��@��@�!@�\@M�@-@J@�@��@x�@G�@��@�u@bN@1'@1'@1'@ �@  @�w@�@�P@K�@�@
=@
=@�@��@E�@�T@@p�@?}@V@V@V@��@��@��@Z@(�@�m@��@t�@
�@
��@
n�@
=q@
J@	��@	��@	X@	�@��@�`@��@��@�@bN@ �@�@�P@l�@;d@
=@�@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ȴAә�A�^5A�VA�K�A�;dA�&�A��A��A�{A��A�bA�1A�1A�  A�A�A�  A���A���A��
A���AҶFAҡ�AҁA�t�A�dZA�`BA�^5A�?}Aѡ�AϸRA�
=A�l�AǅA�x�A�?}A�1A�oA��-A�ȴA�O�A��A��;A�(�A�O�A�p�A���A�A�G�A�A�A�
=A��RA��A���A��A�S�A���A��A���A��mA�dZA��;A��+A��yA�VA�A�A�A��!A��`A�z�A��A�`BA�oA��A��mA�z�A��A�1A��A�z�A�
=A��-A�
=A�dZA�1'A���A�n�A�x�A��7A��uA���A��-A��`A��uA��!A���A�+A��DA�A�t�A�S�A��A��;A���A��AA|VAy|�Au��Ar�`Ap�\AlM�Aj��Aip�Ag�TAg��Af5?Adr�Ab�A`�A^r�A[�^AZ  AWƨAUt�AT$�AR�AP�AN�9AM|�AL��ALffAK
=AJJAHZAFȴAD�!ACoA@��A@-A>^5A<�uA;`BA:5?A8�/A7A6jA5VA3�;A3�A2jA0�A.��A-33A,ffA+�A+�PA+A*ĜA*A)�A(bNA&�!A&-A%��A%��A#�-A#C�A"bNA!"�A �\A�PA��AC�A��AA�\A�-Az�AO�A��A��A  A��Ar�A5?A�#At�A%A�+A�!A
�AdZAdZA
��A
A	C�A5?A�FAl�A/A9XA|�A
=A�DAhsA�A ��A ~�@�;d@��u@�Z@�33@�7L@�z�@�l�@��@��@�S�@@�+@�7@��@��@�|�@�n�@�bN@旍@�O�@�(�@�w@◍@��@�|�@�@�t�@��T@��@�&�@�`B@�x�@݉7@�J@��H@�C�@ߥ�@ߥ�@۾w@�Q�@�S�@թ�@���@Լj@ӶF@ӍP@�9X@�l�@�l�@�@ύP@�%@�C�@���@�X@�K�@�/@���@�O�@��y@�=q@ɡ�@ȣ�@�9X@Ǖ�@� �@���@�dZ@��@���@�G�@�l�@�K�@°!@�~�@+@���@���@��j@��D@���@���@�S�@��@��+@�$�@��#@��-@�-@��@��;@�ff@���@�hs@�`B@���@��j@�5?@��@��w@�dZ@�;d@�@��H@���@�-@��@�%@�Ĝ@���@�bN@�r�@�9X@��m@�1@���@�S�@��@�33@���@�V@���@��#@��-@��@��@��@�l�@�K�@���@�M�@��/@��!@���@�p�@�?}@�V@��`@�(�@�33@��y@���@���@��\@�~�@�^5@�-@�J@��-@��@�Ĝ@�Q�@�1'@�t�@�^5@�X@�j@��P@�V@�@��@�j@��@��;@��w@���@�V@�j@��F@�S�@��D@�|�@���@��-@���@�x�@�p�@�O�@�7L@��@�j@�1'@�A�@�b@���@���@�E�@��@��`@�r�@���@�l�@�33@�
=@�o@�o@���@���@�$�@��@���@���@��7@�7L@��@��/@���@�j@�b@��@�;d@�"�@�
=@��@�ȴ@��!@���@�~�@�^5@�V@�E�@�{@���@���@��h@�`B@��@���@��D@�r�@�j@�I�@�1'@�(�@�  @��m@��
@���@�ƨ@��w@��@�C�@��@��H@��@��@��!@��\@��+@�V@�$�@���@���@�p�@�G�@�V@�%@��@���@��j@��@��9@��9@��9@�j@�(�@��@��F@�"�@���@���@���@�v�@�V@��@���@��-@�X@�V@���@��@��u@�bN@�A�@�  @�ƨ@���@��@�l�@�+@�
=@���@�~�@�J@�J@�J@��@�p�@�O�@��@���@���@�r�@�A�@�b@~�y@~v�@~5?@}�@}�h@}V@|�@|z�@|I�@|1@{S�@z�@z�!@z��@z~�@zn�@zn�@zM�@y��@y&�@xĜ@x �@w�@w\)@v�R@vE�@u�@u`B@u�@t�@t�j@tZ@s��@s�F@sdZ@s33@r��@r^5@r=q@rJ@q&�@pA�@pb@o�;@o|�@n�y@n�R@nV@n$�@m��@m`B@l�/@l�D@lZ@l(�@k��@k�
@kƨ@k�F@k33@j��@jn�@jM�@jJ@i�^@ix�@iG�@hĜ@h�u@h�@h�@hA�@g�@g�w@gl�@g�@g
=@f�y@f�@f��@f{@e�@e@ep�@e/@d��@d�/@d�@dz�@d9X@c��@c��@c�@cdZ@c33@b�@b~�@b^5@b�@a�7@aG�@a&�@`��@`�u@`A�@_�w@_l�@_+@^�R@^{@]�@]/@]V@\��@\Z@[�
@[t�@[S�@[33@Z�@Z~�@Z-@ZJ@Y�^@YX@X��@X�9@X�u@XQ�@W��@W|�@WK�@W;d@V��@V�R@V�@Vȴ@V�+@V5?@T�@S�m@S�F@S��@R�@R�!@R^5@R�@Qx�@P��@PbN@PQ�@P1'@P  @O|�@O�@Nȴ@N��@Nv�@NV@N{@M�T@M�-@M`B@L�@L�D@L9X@Kƨ@Kt�@K@J=q@I�#@Ix�@H��@H�@HQ�@G�@G�@G�P@G|�@Gl�@G;d@Fȴ@Fv�@FE�@E@Ep�@E?}@D�j@D�@C��@C�
@C��@CS�@C@B��@B~�@B�@B-@B-@B-@B�@A�#@A�^@A��@AX@@��@@��@@bN@@b@?|�@?;d@?�@?�@>�@>ff@=��@=�h@=`B@=?}@=/@=V@<�/@<j@<1@;t�@;"�@;"�@;o@:�@:��@9��@9�^@9��@9�@8�u@81'@7��@7l�@7�@6��@6�@6��@6E�@6{@5�T@5@5�h@5�@4�j@4j@4I�@4(�@41@3�
@3t�@3S�@3o@2��@2�!@2^5@1��@1��@1G�@17L@17L@1&�@0��@0�9@0�@0bN@0 �@/�w@/��@/l�@/K�@/;d@/�@.�y@.�R@.��@.v�@.V@.5?@.$�@.@-p�@,��@,��@,��@,�/@,��@,1@+�F@+��@+�@+dZ@*�@*��@*�\@*^5@)��@)�^@)X@)%@(��@(A�@(b@'�w@'l�@'K�@'+@&�@&ff@&E�@&$�@%�@%?}@$�@$��@$��@$z�@$9X@$�@#��@#�
@#�@#t�@#C�@#@"�!@"M�@"-@!�@!��@!��@!�7@!x�@!&�@ ��@ �9@ �u@ r�@  �@  �@ b@ b@��@\)@+@
=@��@��@v�@E�@$�@@�h@`B@?}@�@�@�j@��@�D@z�@I�@(�@�
@�F@��@t�@"�@o@�@��@��@n�@M�@-@��@��@��@��@x�@G�@��@�u@r�@bN@1'@  @�;@�w@��@l�@K�@��@�y@�+@v�@5?@�@�T@��@��@`B@/@V@�@�D@I�@(�@�m@�F@�@C�@�@�H@��@��@�!@�\@M�@-@J@�@��@x�@G�@��@�u@bN@1'@1'@1'@ �@  @�w@�@�P@K�@�@
=@
=@�@��@E�@�T@@p�@?}@V@V@V@��@��@��@Z@(�@�m@��@t�@
�@
��@
n�@
=q@
J@	��@	��@	X@	�@��@�`@��@��@�@bN@ �@�@�P@l�@;d@
=@�@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@�B@�B@�BB�BB�BB�B@�B@�B@�B@�B@�B@�B@�BA�B@�BA�BC�BD�B@�BD�BH�BH�BH�BH�BH�BR�B[#B\)B]/BbNB]/BO�BffB�B�PB��B�B�XBĜB��B�BB�B�B{BbBB��B�BK�BVBYBZB[#B^5BaHBaHB_;BW
BO�BL�BE�BQ�BVBT�BK�BM�BN�BF�B@�B=qB9XB33B49B2-B-B)�B$�B�BDB%BBBB��B�B�NB�B�XB�VB|�BhsBVBG�B+B�BJB
��B
�ZB
ĜB
�B
��B
�bB
v�B
cTB
W
B
A�B
(�B
\B	��B	�#B	ǮB	�B	��B	�{B	�7B	�B	~�B	s�B	ffB	YB	K�B	;dB	1'B	!�B	oB		7B	B�B�B�TB�BB�)B�B��BǮB�}B�LB�!B��B��B��B��B��B�hB�JB�1B�B� B{�By�Bx�Bu�Bq�Bm�Bk�BjBiyBiyBgmBhsBe`Be`BbNB`BB_;B_;B^5B[#B[#BXBVBT�BO�BG�B@�B<jB9XB8RB6FB33B33B1'B1'B/B/B/B0!B49B6FB:^B>wB0!BH�BR�BO�BK�BJ�BR�BYBYBYBW
BR�BS�BT�BW
BYBZB\)B_;B[#BS�BXBZBYBZBXBYB[#BbNBl�Bo�Br�Br�Bt�Bs�Bs�Bq�Bp�Bo�Bp�Bp�Br�By�Bz�Bw�Bw�Bx�B�B�+B�DB�VB�{B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�B�B��B��B�\B�PB�DB��B��B��B�RB�LB�?B�9B�-B�9B�3B�qBÖBƨBǮB��B��B��B��B��B�B�/B�BB�ZB�sB�B�B�B�B�B�B��B��B��B	B	B	%B	B	B	B		7B	DB	PB	�B	"�B	-B	1'B	1'B	1'B	1'B	49B	5?B	49B	8RB	:^B	:^B	A�B	G�B	J�B	J�B	K�B	L�B	O�B	O�B	Q�B	S�B	W
B	W
B	W
B	XB	ZB	[#B	\)B	\)B	^5B	`BB	aHB	`BB	[#B	YB	XB	XB	XB	XB	YB	[#B	]/B	_;B	dZB	gmB	hsB	iyB	jB	k�B	l�B	m�B	n�B	o�B	o�B	o�B	n�B	n�B	l�B	k�B	jB	l�B	q�B	r�B	s�B	u�B	w�B	w�B	v�B	s�B	r�B	r�B	|�B	}�B	z�B	z�B	y�B	y�B	y�B	y�B	y�B	y�B	|�B	|�B	}�B	�B	�B	�B	�B	�1B	�1B	�=B	�+B	�+B	�%B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�JB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�XB	�^B	�dB	�dB	�jB	�jB	�jB	�qB	��B	B	B	B	B	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
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
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
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
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
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
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
$�B
$�B
%�B
%�B
%�B
&�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
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
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
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
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
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
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
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
W
B
W
B
XB
W
B
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
[#B
[#B
[#B
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
_;B
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
gmB
gmB
gmB
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
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@�B@�B@iBBuBBuBBuB@iB@OB@OB@OB@iB@iB@OBAUB@OBAUBC{BD�B@iBDgBH�BH�BH�BH�BH�BR�BZ�B\CB]�BdtBa�BVBmB�+B��B��B��B�VB�1B�EB�cB�B�B!-BSB:BmB��BBM�BXyB[�B[�B]B_�Bb�BbB`vBYKBS[BOvBF�BR�BW?BV�BMjBOBP�BHBAUB>wB:^B3�B5ZB3MB.B+6B&�B�BB�B�B�B�B�6B�cB�zB�IB��B��B~�BjBW�BJ=B-B]B�B�B
��B
ǔB
��B
��B
��B
y>B
e`B
Z7B
EmB
,qB
uB	�B	�5B	˒B	��B	�;B	��B	��B	��B	��B	u�B	h�B	\B	N�B	=�B	3�B	$@B	B	B	MB��B�B��B�BݲB�YB��BɠB��B�>B�GB�*B��B��B��B��B��B��B��B��B�;B|�Bz�Bz�Bw�BshBnIBk�Bj�Bi�Bi�Bh>Bi_BffBf�Bb�B`�B_�B`�B^�B\)B\]BX�BW�BWYBRoBI7BAUB=B:xB9�B7�B4�B49B1�B2-B/OB/OB/iB0�B4�B7LB<jB@ B/�BH�BS�BP�BL�BK�BS[BYKBYBZBW�BS�BT�BV�BX�BY�BZ�B]IB`�B]/BT�BX�BZQBY�BZ�BX�BZQB[WBbhBl�Bo�BsBr�ButBt�Bt�BrGBqBo�Bp�BpUBr�Bz�B|Bx�BxBx�B��B��B��B��B��B�;B��B�yB�B��B�B�IB��B��B��B�xB�IB�B��B��B�qB� B�_B�vB�PB��B�B��B�B�XB�fB�tB��B�-B�B��B�qBÖBƨBǮB͟B̈́B��B��B��B��B�dB�vB�@B�sB�B�B�B�B�B�B��B��B��B	�B	�B	�B	MB	�B	�B		7B	
�B	JB	�B	"4B	,�B	0�B	1B	1B	1B	4TB	5�B	4B	8RB	:xB	9�B	A;B	G�B	J�B	JrB	K�B	L�B	O�B	O�B	RB	S�B	W
B	V�B	V�B	W�B	ZQB	[qB	\B	\B	^5B	`�B	bB	a-B	[qB	X�B	W�B	W�B	W�B	XEB	YKB	[	B	\�B	_!B	d&B	g8B	h>B	iDB	jeB	k�B	l�B	m�B	n�B	o�B	o�B	p!B	oB	n�B	l�B	k�B	jB	l�B	q�B	r�B	s�B	u�B	w�B	x�B	w�B	s�B	raB	q�B	}qB	~wB	z�B	z�B	y�B	y�B	y�B	y�B	y�B	y�B	|�B	|�B	}�B	�B	�AB	� B	�9B	�KB	�KB	�XB	�B	��B	��B	��B	��B	�B	�B	�%B	��B	�B	�	B	�0B	�6B	�NB	�@B	�FB	�mB	��B	��B	��B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�+B	�2B	�B	�$B	�$B	�$B	�$B	�	B	�B	�0B	�0B	�6B	�6B	�PB	�qB	�iB	�[B	�[B	�[B	�AB	�aB	�gB	�gB	�mB	ǔB	ȚB	ɺB	�~B	͟B	ΊB	ΥB	ϫB	ϫB	ЗB	ЗB	ЗB	бB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�;B	�-B	�B	�B	� B	�:B	�&B	�@B	�FB	�2B	�B	�B	�RB	�8B	�RB	�sB	�_B	�B	�KB	�eB	�B	�=B	�qB	�]B	�wB	�B	�iB	�B	�B	�|B	�|B	�|B	�B	��B	�B	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
%B
B
�B
�B
	B
	B

	B

#B
B
)B
B
0B
B
B
B
B
B
B
B
B
<B
(B
B
(B
B
.B
B
.B
4B
B
B
4B
4B
:B
:B
:B
&B
&B
&B
@B
[B
,B
,B
,B
MB
2B
2B
MB
9B
9B
SB
SB
YB
?B
?B
?B
YB
_B
EB
yB
KB
KB
KB
eB
�B
kB
kB
WB
qB
qB
�B
dB
~B
~B
�B
�B
�B
�B
�B
�B
�B
jB
�B
�B
�B
 vB
 �B
 �B
 �B
 �B
!|B
!�B
!�B
!|B
!�B
!|B
"�B
$�B
$�B
%�B
%�B
%�B
&�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
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
+�B
+�B
+�B
+�B
+�B
+�B
-B
-�B
-�B
-�B
/ B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
1�B
2B
2�B
2�B
4B
4B
4�B
5B
5B
5B
6B
6B
6B
5�B
6�B
6�B
6�B
6�B
7B
7B
7B
6�B
72B
8B
8B
88B
88B
9$B
9$B
:*B
:B
:DB
:DB
;B
;B
;B
;B
;0B
;0B
;JB
<PB
<PB
="B
="B
=<B
=<B
=<B
>]B
>BB
>BB
>]B
?HB
?HB
@4B
@OB
A;B
AUB
A;B
A;B
B[B
BAB
BAB
BAB
B[B
CaB
CGB
DMB
DgB
DMB
DMB
DgB
DgB
DgB
EmB
EmB
ESB
E�B
F�B
FYB
FtB
G_B
G_B
GzB
G_B
G_B
H�B
HfB
H�B
H�B
H�B
HfB
I�B
IlB
I�B
I�B
I�B
I�B
IlB
I�B
J�B
J�B
JrB
J�B
J�B
KxB
KxB
KxB
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L~B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
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
V�B
V�B
W�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
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
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
^B
^B
_B
^�B
_B
_B
_B
^�B
`B
`B
`B
_�B
`�B
aB
aB
`�B
aB
aB
`�B
aB
bB
a�B
bB
cB
cB
cB
d&B
c�B
dB
dB
dB
d&B
dB
e,B
eB
e,B
e,B
fB
f2B
fLB
f2B
g8B
gB
gB
gB
gB
g8B
g8B
gB
gB
h>B
h$B
h
B
h$B
h>B
h>B
i_B
i*B
j0B
jKB
jKB
jKB
jB
k6B
jKB
kQB
kQB
kQB
k6B
kQB
lWB
lWB
lqB
mCB
m]B
m]B
m]B
nIB
ncB
n}B
oOB
oOB
oOB
oOB
oiB
oiB
oOB
poB
pUB
pUB
q[B
qvB
qvB
raB
r|B
r|B
ra11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.67(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911120037322019111200373220191112003732202306231718502023062317185020230623171850201911130030562019111300305620191113003056  JA  ARFMdecpA19c                                                                20191107153719  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191107063746  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191107063748  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191107063748  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191107063749  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191107063749  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191107063749  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191107063749  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191107063750  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191107063750                      G�O�G�O�G�O�                JA  ARUP                                                                        20191107065503                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191107153500  CV  JULD            G�O�G�O�F�P                JM  ARCAJMQC2.0                                                                 20191111153732  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191111153732  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191112153056  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081850  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                