CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-18T00:35:14Z creation;2017-11-18T00:35:18Z conversion to V3.1;2019-12-19T07:56:25Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171118003514  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_180                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�63� � 1   @�64}'Ҁ@;�TɅ�o�de4�J�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%�3C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<l�D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHy�DH��DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd��Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do��Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�|�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aħ�Aģ�Aħ�Aħ�Aĥ�Aħ�Aħ�Aĩ�Aĩ�Aĩ�Aħ�Aĩ�Aĩ�Aħ�Aħ�AĮAħ�AĮAħ�Aĩ�AĮAĥ�Aĥ�Ağ�Aĝ�Ağ�Aħ�Aė�A�ZA��9A�\)A�ĜA��HA�5?A���A��RA���A��yA���A���A�bNA���A�I�A�|�A��A��RA�r�A��A�~�A��FA�?}A���A��A�r�A�n�A� �A�O�A��A��\A�?}A�1A��`A�t�A��A��7A�hsA��jA��A��-A��A�bA��+A���A��DA�G�A�+A���A��yA�|�A�ƨA�ZA��;A���A�r�A�O�A�  A~9XA|-Az��Az�Ay��Ay�Aw7LAu"�AtffAtVAt9XAs
=Ar1AqƨAqdZAp=qAm�Ak��AkC�Aj1Ai;dAhn�Ag�-Ag?}Af��Af~�Ae�^Ae%Ad��Ac�;Acx�AcVAa�-A`��A`��A_hsA^�9A]dZA\��A\~�A\M�A[x�AZ9XAXffAW&�AV=qAU��AU�AT�jAT~�ATI�AS��AQ�AQ��AQ��AP1'AN{AM��AM�AMp�AM+AL�yAL��AL�AK�7AJĜAJ9XAIƨAH��AHbAFn�AE�AD��ACAA�mA@ȴA@^5A@{A?p�A>��A=t�A<�DA;�A;��A;|�A;C�A:ZA9dZA8-A7O�A6v�A6 �A6  A5��A5dZA533A4��A4�A4�A2��A1�PA0�jA01A/C�A.ȴA.{A,�!A+%A*��A*ffA*�A)�^A)/A(M�A'��A'�A&I�A$�A#�A#x�A"��A"I�A!��A ��A r�A =qA�
A�Ar�At�A�RAI�Al�AA�A��AbNA��AVAffA�mA�`A�uA�A�PAdZA��A�AjA��AA�jA�AJA�\A��A\)A�A
��A
�jA	�A��AȴAv�AZAĜA1'A��AA�A z�@�t�@��@�`B@��D@� �@���@�S�@���@���@��@�b@���@��\@�~�@�n�@�ff@�=q@�@�@��@�^@�@�j@�w@�=q@땁@�@�Q�@柾@�Ĝ@�@޸R@��@��@�hs@��/@أ�@�Z@׍P@�ff@�@�O�@ԃ@��H@�J@̛�@̋D@�1'@���@�M�@�5?@��#@Ɂ@���@�j@���@ǍP@��@�^5@�z�@���@�S�@�@�33@��w@���@�Q�@�|�@��@��H@���@�@��@�A�@�(�@���@���@��P@�\)@�@���@�$�@�/@��@�\)@���@�hs@�&�@���@��9@��m@�@�V@��@��@�x�@�&�@�9X@���@�+@�dZ@���@�@�-@��T@��#@���@���@���@��@�ff@��@��@�z�@�9X@�b@��
@�t�@��@�V@�O�@��9@�(�@��@��w@�|�@�S�@�@�^5@���@�@���@���@�`B@���@�9X@�
=@�=q@�{@�J@�J@���@��#@�hs@� �@�@�
=@�dZ@�|�@��@�+@�-@��T@�%@���@���@���@���@��@��u@��;@�l�@��@��H@�~�@���@�?}@��/@�j@��@���@���@�hs@���@���@��@�I�@�9X@��F@�o@���@���@�M�@�-@�-@�-@��@�J@�J@�{@��@�{@�@��^@��D@�b@��@��@~�@|�j@|(�@{��@{��@{dZ@{C�@{"�@{@z��@z�\@z�@yx�@y%@xr�@v�R@u�T@u@u�@uO�@t�D@s��@st�@sS�@so@r��@r�\@r=q@r=q@r�@rJ@rJ@rJ@rJ@q��@q��@q��@q�#@q�^@q�^@q�^@q�7@qX@q&�@qhs@q&�@q%@q�@p�`@pb@ol�@o+@nv�@m�@m�@mV@l�@mV@m/@mV@l��@l�/@l�/@l��@l�D@l9X@k�
@k��@k��@k�@kt�@kt�@kt�@kdZ@kS�@k33@ko@k@k@k@j�H@j�H@k33@k@k�F@l1@k��@kdZ@kt�@i��@i��@iG�@hA�@gl�@f�@f�+@fV@f$�@f@e�-@d��@d��@d(�@c�
@c�F@c��@cC�@c@co@b�@b�H@b��@b��@bn�@b�@a�@a%@`A�@_�;@_�P@_l�@_+@^ȴ@^��@^V@^@^$�@`A�@bJ@b�\@a��@_��@]@]p�@\Z@\1@[�m@\z�@\�j@]�@^v�@^5?@]�@]�@]�@]?}@[�
@[t�@Z^5@Y��@Y��@Y&�@XĜ@Xr�@W\)@V�y@V��@VV@V{@U@U��@U�h@U?}@T�/@T�j@T9X@S�F@S��@SS�@S33@So@S@R��@R�\@Rn�@R=q@Q��@Q�7@P��@P�9@PbN@O�@O�w@O;d@N�R@N��@Nv�@NE�@M�T@M��@MO�@L��@L��@L1@Kt�@Kt�@K33@J�@I�#@I�^@Ix�@I�@H��@H�9@H  @G�;@G��@G�@G|�@G;d@F�R@FV@E��@D�j@DI�@D1@C�m@C�F@C��@Ct�@B��@Ahs@A�@@�@?�;@?�@?��@?+@?�@?�@>�y@>��@>E�@>@=@=O�@=V@<Z@<I�@<(�@;�
@;�F@:��@:�\@:~�@:^5@:^5@:J@9��@9�7@9X@9�@8  @7�P@7+@7�@6��@6�y@6ȴ@6��@6��@6��@5@5�@4�/@4�@4�@3�m@3�
@3�F@3�F@3�F@3��@3t�@3"�@2�!@1��@1%@0��@0Q�@01'@/�w@/�P@/\)@/+@.�y@.�@.��@.V@.{@-�@-�@-�T@-�T@-�T@-�h@-�@-�@-�h@-�h@-�h@-�h@-�h@-�@-�@-�h@-�@-�@-O�@,��@+�m@+�F@+t�@+t�@+C�@*�@*�H@*�H@*�!@*M�@*-@*J@)�@)��@)��@)��@)x�@)X@(�`@(��@(�@'�;@'+@&5?@%��@%��@%�h@%�h@%p�@%`B@%`B@%?}@%/@%�@%V@$��@$�@$�/@$�@$1@#t�@#"�@#"�@#"�@"�H@"�\@"-@!hs@!%@ ��@  �@   @�;@�w@��@K�@�@�y@ȴ@�R@V@$�@�T@�-@�@p�@?}@�@V@�@��@�j@�D@Z@1@�F@�H@M�@�@�@�^@�7@G�@&�@%@��@�`@��@bN@  @�@�@�P@��@V@5?@�T@��@�-@�@@�-@`B@�/@��@�j@�D@9X@1@��@�
@��@��@��@�@�@dZ@"�@�@��@��@~�@n�@=q@�@J@��@�@�#@�7@x�@%@��@��@�u@�u@r�@bN@A�@ �@b@b@�@��@�@+@�y@�y@�@ȴ@ff@$�@$�@@�@��@@��@O�@��@j@��@�
@�F@��@t�@"�@
��@
��@
n�@
=q@
J@	�#@	��@	hs@	G�@	%@�`@Ĝ@�u@r�@bN@Q�@ �@�@��@��@�@�P@|�@|�@l�@\)@+@
=@��@�@�R@��@�+@�+@V@5?@$�@�T@��@�h@O�@�@�@�/@��@�j@�@z�@9X@�F@33@o@��@�\@~�@^5@=q@�@�@��@��@7L@&�@�@%@ ��@ Ĝ@ �9@ ��@ �u@ r�@ A�@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aħ�Aģ�Aħ�Aħ�Aĥ�Aħ�Aħ�Aĩ�Aĩ�Aĩ�Aħ�Aĩ�Aĩ�Aħ�Aħ�AĮAħ�AĮAħ�Aĩ�AĮAĥ�Aĥ�Ağ�Aĝ�Ağ�Aħ�Aė�A�ZA��9A�\)A�ĜA��HA�5?A���A��RA���A��yA���A���A�bNA���A�I�A�|�A��A��RA�r�A��A�~�A��FA�?}A���A��A�r�A�n�A� �A�O�A��A��\A�?}A�1A��`A�t�A��A��7A�hsA��jA��A��-A��A�bA��+A���A��DA�G�A�+A���A��yA�|�A�ƨA�ZA��;A���A�r�A�O�A�  A~9XA|-Az��Az�Ay��Ay�Aw7LAu"�AtffAtVAt9XAs
=Ar1AqƨAqdZAp=qAm�Ak��AkC�Aj1Ai;dAhn�Ag�-Ag?}Af��Af~�Ae�^Ae%Ad��Ac�;Acx�AcVAa�-A`��A`��A_hsA^�9A]dZA\��A\~�A\M�A[x�AZ9XAXffAW&�AV=qAU��AU�AT�jAT~�ATI�AS��AQ�AQ��AQ��AP1'AN{AM��AM�AMp�AM+AL�yAL��AL�AK�7AJĜAJ9XAIƨAH��AHbAFn�AE�AD��ACAA�mA@ȴA@^5A@{A?p�A>��A=t�A<�DA;�A;��A;|�A;C�A:ZA9dZA8-A7O�A6v�A6 �A6  A5��A5dZA533A4��A4�A4�A2��A1�PA0�jA01A/C�A.ȴA.{A,�!A+%A*��A*ffA*�A)�^A)/A(M�A'��A'�A&I�A$�A#�A#x�A"��A"I�A!��A ��A r�A =qA�
A�Ar�At�A�RAI�Al�AA�A��AbNA��AVAffA�mA�`A�uA�A�PAdZA��A�AjA��AA�jA�AJA�\A��A\)A�A
��A
�jA	�A��AȴAv�AZAĜA1'A��AA�A z�@�t�@��@�`B@��D@� �@���@�S�@���@���@��@�b@���@��\@�~�@�n�@�ff@�=q@�@�@��@�^@�@�j@�w@�=q@땁@�@�Q�@柾@�Ĝ@�@޸R@��@��@�hs@��/@أ�@�Z@׍P@�ff@�@�O�@ԃ@��H@�J@̛�@̋D@�1'@���@�M�@�5?@��#@Ɂ@���@�j@���@ǍP@��@�^5@�z�@���@�S�@�@�33@��w@���@�Q�@�|�@��@��H@���@�@��@�A�@�(�@���@���@��P@�\)@�@���@�$�@�/@��@�\)@���@�hs@�&�@���@��9@��m@�@�V@��@��@�x�@�&�@�9X@���@�+@�dZ@���@�@�-@��T@��#@���@���@���@��@�ff@��@��@�z�@�9X@�b@��
@�t�@��@�V@�O�@��9@�(�@��@��w@�|�@�S�@�@�^5@���@�@���@���@�`B@���@�9X@�
=@�=q@�{@�J@�J@���@��#@�hs@� �@�@�
=@�dZ@�|�@��@�+@�-@��T@�%@���@���@���@���@��@��u@��;@�l�@��@��H@�~�@���@�?}@��/@�j@��@���@���@�hs@���@���@��@�I�@�9X@��F@�o@���@���@�M�@�-@�-@�-@��@�J@�J@�{@��@�{@�@��^@��D@�b@��@��@~�@|�j@|(�@{��@{��@{dZ@{C�@{"�@{@z��@z�\@z�@yx�@y%@xr�@v�R@u�T@u@u�@uO�@t�D@s��@st�@sS�@so@r��@r�\@r=q@r=q@r�@rJ@rJ@rJ@rJ@q��@q��@q��@q�#@q�^@q�^@q�^@q�7@qX@q&�@qhs@q&�@q%@q�@p�`@pb@ol�@o+@nv�@m�@m�@mV@l�@mV@m/@mV@l��@l�/@l�/@l��@l�D@l9X@k�
@k��@k��@k�@kt�@kt�@kt�@kdZ@kS�@k33@ko@k@k@k@j�H@j�H@k33@k@k�F@l1@k��@kdZ@kt�@i��@i��@iG�@hA�@gl�@f�@f�+@fV@f$�@f@e�-@d��@d��@d(�@c�
@c�F@c��@cC�@c@co@b�@b�H@b��@b��@bn�@b�@a�@a%@`A�@_�;@_�P@_l�@_+@^ȴ@^��@^V@^@^$�@`A�@bJ@b�\@a��@_��@]@]p�@\Z@\1@[�m@\z�@\�j@]�@^v�@^5?@]�@]�@]�@]?}@[�
@[t�@Z^5@Y��@Y��@Y&�@XĜ@Xr�@W\)@V�y@V��@VV@V{@U@U��@U�h@U?}@T�/@T�j@T9X@S�F@S��@SS�@S33@So@S@R��@R�\@Rn�@R=q@Q��@Q�7@P��@P�9@PbN@O�@O�w@O;d@N�R@N��@Nv�@NE�@M�T@M��@MO�@L��@L��@L1@Kt�@Kt�@K33@J�@I�#@I�^@Ix�@I�@H��@H�9@H  @G�;@G��@G�@G|�@G;d@F�R@FV@E��@D�j@DI�@D1@C�m@C�F@C��@Ct�@B��@Ahs@A�@@�@?�;@?�@?��@?+@?�@?�@>�y@>��@>E�@>@=@=O�@=V@<Z@<I�@<(�@;�
@;�F@:��@:�\@:~�@:^5@:^5@:J@9��@9�7@9X@9�@8  @7�P@7+@7�@6��@6�y@6ȴ@6��@6��@6��@5@5�@4�/@4�@4�@3�m@3�
@3�F@3�F@3�F@3��@3t�@3"�@2�!@1��@1%@0��@0Q�@01'@/�w@/�P@/\)@/+@.�y@.�@.��@.V@.{@-�@-�@-�T@-�T@-�T@-�h@-�@-�@-�h@-�h@-�h@-�h@-�h@-�@-�@-�h@-�@-�@-O�@,��@+�m@+�F@+t�@+t�@+C�@*�@*�H@*�H@*�!@*M�@*-@*J@)�@)��@)��@)��@)x�@)X@(�`@(��@(�@'�;@'+@&5?@%��@%��@%�h@%�h@%p�@%`B@%`B@%?}@%/@%�@%V@$��@$�@$�/@$�@$1@#t�@#"�@#"�@#"�@"�H@"�\@"-@!hs@!%@ ��@  �@   @�;@�w@��@K�@�@�y@ȴ@�R@V@$�@�T@�-@�@p�@?}@�@V@�@��@�j@�D@Z@1@�F@�H@M�@�@�@�^@�7@G�@&�@%@��@�`@��@bN@  @�@�@�P@��@V@5?@�T@��@�-@�@@�-@`B@�/@��@�j@�D@9X@1@��@�
@��@��@��@�@�@dZ@"�@�@��@��@~�@n�@=q@�@J@��@�@�#@�7@x�@%@��@��@�u@�u@r�@bN@A�@ �@b@b@�@��@�@+@�y@�y@�@ȴ@ff@$�@$�@@�@��@@��@O�@��@j@��@�
@�F@��@t�@"�@
��@
��@
n�@
=q@
J@	�#@	��@	hs@	G�@	%@�`@Ĝ@�u@r�@bN@Q�@ �@�@��@��@�@�P@|�@|�@l�@\)@+@
=@��@�@�R@��@�+@�+@V@5?@$�@�T@��@�h@O�@�@�@�/@��@�j@�@z�@9X@�F@33@o@��@�\@~�@^5@=q@�@�@��@��@7L@&�@�@%@ ��@ Ĝ@ �9@ ��@ �u@ r�@ A�@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B��B��B�oBl�BJ�B�B�B%B�B�
B�5B�B�;B��B�B��B�%B_;Bo�Bn�Be`B`BBVBI�B;dB(�B!�B'�B&�B%�B#�B�B�BuBB
��B
��B
�B
�B
�/B
�#B
��B
��B
��B
ȴB
B
�?B
�-B
��B
��B
��B
��B
��B
��B
�VB
�B
u�B
n�B
k�B
hsB
bNB
W
B
H�B
I�B
K�B
H�B
?}B
5?B
7LB
33B
(�B
�B
\B
JB
%B
  B	��B	��B	��B	��B	�B	�B	�mB	�`B	�BB	�5B	�B	��B	��B	��B	B	�wB	�LB	�FB	�LB	�9B	�B	��B	��B	��B	�uB	�oB	�hB	�JB	�JB	�7B	�B	x�B	{�B	x�B	o�B	aHB	gmB	hsB	hsB	ffB	dZB	bNB	^5B	[#B	W
B	S�B	Q�B	J�B	D�B	<jB	49B	5?B	-B	!�B	�B	!�B	�B	�B	oB	bB	DB	JB	DB	
=B	+B	  B��B�B�B�B�B�B�B�B�B�B�fB�TB�B��B�B��B��B��BɺBB�jB��BB��B�qB�XB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�DB�+B�1B�B{�Bz�Bu�Bt�Bu�Bq�Bp�BjBm�Bk�BgmBffB`BBXBZBYBXBZBYBR�BG�BM�BO�BP�BN�BK�BD�B<jB7LBB�BB�B9XB9XB9XB.B(�B2-B33B7LB7LB9XB9XB7LB5?B7LB6FB:^B;dB8RB<jB<jB;dB:^B6FB/B.B49B33B0!B-B.B+B.B.B33B.B0!B&�B#�B0!B2-B49B6FB5?B49B33B7LB7LB5?B0!B%�B2-B:^B9XB7LB=qBA�B?}B@�B@�BB�BC�BD�BD�BB�B>wB<jB6FB8RB49B49B9XB?}BB�BE�BF�BF�BC�BD�BJ�BK�BK�BJ�BJ�BJ�BI�BH�BG�BE�BG�BF�BJ�BQ�BT�BVBT�BT�BW
BYBZB\)BaHBcTBcTBiyBl�Bp�Br�Bq�Br�Bt�Bt�Br�Bo�Bq�Bv�Bx�Bv�Bz�B|�B}�B~�B~�B� B� B�B�B�bB��B��B��B��B��B��B��B��B�B�-B�9B�9B�RB��BɺB��B�#B�)B�)B�)B�#B�#B�5B�TB�yB��B��B��B��B��B	  B	B	%B	+B	1B	1B	+B	%B	B	+B	+B		7B		7B	1B		7B	
=B	DB	
=B	PB	oB	uB	{B	�B	�B	�B	�B	�B	!�B	#�B	#�B	$�B	(�B	+B	-B	/B	0!B	0!B	1'B	1'B	2-B	2-B	1'B	/B	49B	6FB	8RB	7LB	7LB	B�B	D�B	E�B	F�B	H�B	H�B	I�B	I�B	I�B	I�B	I�B	J�B	K�B	L�B	T�B	ZB	ZB	ZB	[#B	]/B	aHB	bNB	cTB	cTB	dZB	e`B	gmB	gmB	hsB	iyB	jB	jB	k�B	k�B	k�B	k�B	l�B	o�B	p�B	q�B	s�B	v�B	x�B	z�B	{�B	|�B	}�B	|�B	|�B	|�B	{�B	{�B	~�B	� B	�B	�B	�+B	�7B	�VB	�bB	�bB	�bB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�3B	�'B	�-B	�3B	�9B	�9B	�9B	�9B	�3B	�3B	�?B	�LB	�RB	�XB	�XB	�XB	�XB	�jB	�qB	�wB	�wB	�}B	�wB	�}B	�}B	�}B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	�
B	�
B	�B	��B	��B	��B	��B	��B	�
B	�#B	�;B	�NB	�NB	�TB	�mB	�sB	�mB	�fB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
1B
+B
+B
+B
+B
+B
%B
+B
1B
	7B
	7B

=B

=B
	7B
1B
1B

=B
DB
DB
JB
VB
\B
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
!�B
"�B
"�B
"�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
!�B
"�B
$�B
%�B
$�B
&�B
&�B
&�B
'�B
'�B
&�B
&�B
&�B
%�B
%�B
&�B
'�B
(�B
)�B
(�B
)�B
+B
+B
+B
,B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
0!B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
2-B
2-B
1'B
33B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
7LB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
9XB
:^B
;dB
:^B
:^B
:^B
=qB
>wB
?}B
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
>wB
@�B
A�B
A�B
A�B
@�B
@�B
?}B
@�B
@�B
B�B
D�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
E�B
D�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
N�B
M�B
M�B
O�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
P�B
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
W
B
XB
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
[#B
\)B
\)B
\)B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
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
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
jB
k�B
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B�B�B�B�B��B�B��B�B�B�B�B�B��B�B�B�B�B�B�B��B�"B�B�B�B��B��BܒB�XB�BƎB�Br�BR:B#�BkB	�B�lBܒB��B�#B��B��B�3B�=B�0BfLBq[Bo�BgBa�BXBL~B>�B,�B$tB)B'�B&�B$tB�B�BB�B �B
�zB
�B
�CB
ߊB
ܒB
҉B
�vB
�~B
�7B
��B
�LB
��B
�B
��B
��B
�OB
�7B
�?B
��B
�gB
xB
pB
l�B
iDB
c�B
YKB
J�B
J�B
K�B
IRB
@�B
6zB
7�B
4B
*�B
qB
�B
jB
�B
 B	�B	��B	�zB	�ZB	�[B	�B	�XB	�B	�HB	��B	�	B	ӏB	��B	̘B	�B	�}B	��B	�B	��B	��B	�}B	��B	��B	�
B	��B	�&B	�B	�PB	��B	��B	�MB	z�B	|jB	y�B	q�B	c�B	h
B	h�B	h�B	f�B	d�B	b�B	_!B	\B	XB	T�B	R�B	K�B	F%B	>wB	5�B	6+B	.�B	$B	 'B	"NB	pB	�B	�B	�B	~B	B	�B	
�B	�B	oB�^B�MB��B�B�B�B�AB�B�B�"B�B�&B��BյB�?B�B��B��B�BāB�]B�'B�B� B�(B�^B�zB�MB�B�]B�B��B��B��B��B��B��B�eB�B�YB��B�vB��B�fB�B�uB}�B|BwfBu�Bv�Br�Bq�Bk�BnIBlWBhXBgBa�BZB[=BZ7BX�BZ�BY�BTBI�BN�BP�BQhBO\BL�BF%B>]B9�BC-BCaB;dB:xB:�B0!B+6B3MB4TB7�B8B9�B9�B7�B5�B8B72B:�B;�B9	B<�B<�B;�B:�B7B0�B/OB4nB3�B1'B-�B/iB,�B/iB/�B4�B/�B1�B*B&B0�B2�B4�B6�B5�B5B4B7�B8B6FB1�B(�B3B:�B9�B8RB=�BA�B@ BABA BCBDBEBE9BC�B@4B>wB8B9�B6`B6�B:�B@�BCGBFBGBF�BD�BEmBK)BLBLBKBK)BK)BJ=BI7BHKBF�BHfBG�BK�BRoBUMBV�BU�BU�BW�BY�BZ�B\xBa�Bc�Bd&BjBl�Bp�Br�BrGBsMBuBuBs3Bp�Br|Bw�ByrBw�B{JB}<B~BBHBcB�iB��B��B��B� B�
B��B�B��B�B�-B�nB�_B�iB�|B��B��B��B�;BʦBуB�WB�]B�]B�xBیB��B�!B�B�B��B�B�B�dB��B	 iB	�B	?B	+B	KB	fB	zB	�B	�B	�B	�B		�B		�B	�B		�B	
�B	�B	^B	B	�B	�B	�B	�B	�B	�B	B	OB	"NB	$&B	$&B	%FB	)DB	+6B	-]B	/OB	0UB	0;B	1AB	1[B	2aB	2|B	1�B	0!B	4�B	6�B	8�B	7�B	8B	B�B	D�B	E�B	F�B	H�B	H�B	I�B	I�B	J	B	J	B	J#B	KB	L0B	M�B	U�B	ZQB	ZQB	ZkB	[�B	]�B	a|B	b�B	c�B	c�B	d�B	e�B	gmB	g�B	h�B	i�B	j�B	j�B	k�B	k�B	k�B	k�B	l�B	o�B	p�B	q�B	s�B	v�B	x�B	{B	|B	}"B	~BB	}VB	}<B	}<B	|PB	|PB	.B	�4B	�;B	�AB	�EB	�lB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�/B	�iB	�OB	��B	��B	�MB	��B	��B	��B	��B	�nB	�nB	�nB	�nB	��B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ʦB	�VB	ԕB	�$B	��B	��B	��B	�FB	�FB	�2B	�B	�
B	�#B	�B	�NB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�$B	�$B	�B	�*B	�*B	�B	�$B	�*B	�B	�B	�B	�*B	�B	�B	�0B	�6B	�0B	�6B	�"B	�BB	�]B
 4B
;B
;B
oB
AB
GB
aB
YB
KB
_B
_B
_B
zB
zB
�B
�B
�B
	lB
	lB

�B

rB
	�B
�B
�B

�B
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
"�B
"�B
#B
"B
"�B
#B
# B
"NB
# B
#B
#�B
$&B
$B
$B
$B
$B
#B
"4B
# B
%B
&B
%FB
'B
'B
'B
(
B
(
B
'B
'8B
'8B
&2B
&fB
'8B
($B
)DB
*0B
)DB
*0B
+6B
+6B
+6B
,=B
-CB
-CB
.IB
/OB
0;B
0;B
1AB
1AB
0UB
2GB
3MB
3MB
3MB
3MB
3MB
3MB
3hB
3MB
3MB
33B
2aB
2aB
1�B
3�B
5tB
6zB
7fB
7�B
7�B
8lB
8�B
8�B
7�B
9rB
9�B
:�B
:�B
:�B
:�B
:�B
:�B
9�B
:�B
;�B
:�B
:�B
:�B
=�B
>�B
?�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
>�B
>�B
>�B
@�B
A�B
A�B
A�B
@�B
@�B
?�B
@�B
@�B
B�B
D�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
E�B
D�B
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
G�B
G�B
G�B
G�B
G�B
H1B
IB
J�B
J�B
J�B
J�B
K�B
K�B
MB
MB
M�B
NB
N"B
NB
N�B
O�B
OB
N"B
N"B
PB
OB
PB
PB
QB
RB
R B
R B
Q4B
SB
S&B
S&B
S&B
S&B
TB
T,B
T,B
UB
T�B
U2B
UB
U2B
U2B
V9B
VB
VB
W?B
W?B
W$B
W$B
W$B
W$B
W$B
WYB
W?B
WYB
WYB
XEB
Y1B
YB
Y1B
YKB
Y1B
YKB
YKB
Z7B
Z7B
ZQB
ZQB
ZQB
ZQB
[WB
\]B
\CB
\]B
[qB
\]B
]IB
]dB
]IB
]dB
]dB
]dB
]dB
]~B
^�B
^�B
`vB
`vB
`vB
`\B
`vB
`vB
abB
abB
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
dtB
dtB
dtB
d�B
d�B
d�B
ezB
ezB
e�B
ezB
f�B
f�B
f�B
f�B
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
g�B
g�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
k�B
k�B
k�B
k�B
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
m�B
n�B
n�B
n�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711220036332017112200363320171122003633201806221233392018062212333920180622123339201804050429382018040504293820180405042938  JA  ARFMdecpA19c                                                                20171118093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171118003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171118003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171118003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171118003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171118003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171118003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171118003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171118003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171118003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20171118005509                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171118153329  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171118153329  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171118153329  CV  LONGITUDE       G�O�G�O��#)7                JM  ARCAJMQC2.0                                                                 20171121153633  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171121153633  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192938  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033339  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                