CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:33Z creation;2022-06-04T17:38:33Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173833  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               XA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @فp�r(41   @فqʆB@0%�S����clz�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C�C�C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB33CC�fCF  CH  CJ  CL  CM�fCP  CR  CT  CU�fCX  CZ  C\  C^  C_�fCb  Cd�Cf  Ch  Cj  Cl  Cn�Cp�Cr�Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B7��B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B�fgB�fgB���B���B�  B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�fgB�fgB���B���C�3C��C��C��C	��C��C��C��C��C��C��C��C�gC�gC��C�3C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CB  CC�3CE��CG��CI��CK��CM�3CO��CQ��CS��CU�3CW��CY��C[��C]��C_�3Ca��Cc�gCe��Cg��Ci��Ck��Cm�gCo�gCq�gCs��Cu��Cw��Cy��C{�3C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1��D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��gD�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�:A�oA��A�4A��A��A�oA��A��A�{A�A�$A�+A�1A��A��A��A�kA�	A��A�A��A��A��A� �A�"4A�$A�#�A�%FA�'RA�)�A�!bA�kA�!-A�{A�A�{A�
=A�A�
�A�)�A��Aǹ$Aą�A��A��A�ĜA��MA��)A���A��DA�ŢA��A�m�A��A���A���A��A��tA��?A�{JA��vA���A��A�uA���A� �A��{A���A�'RA�~�A�ŢA��FA�kA���A�Y�A�\A���A���A�0�A�\A��A�,qA�d&A���A��A��'A�]�A�چA�+�A�̘A���A��\A���A���A�#�A��}A���A�"hA��A�A�A�A}�.Aw��At>�Ar:*An�AfJ�A^�AY%AVcAU+AT%ARYKAO�AN�AJԕAH��AG@�AD�AC�AC{AA:�A>��A=U2A=jA<B[A;�_A;>�A9\)A7oiA6?�A5�/A5��A5�)A6��A5xA3�+A3@OA2��A2d�A1�A0"hA,�A,|�A,�`A,�A*f�A+2�A+�AA+7A({�A&}VA&˒A&�A&y�A%��A%uA#��A#A"/�A"H�A"u�A">�A!��A!,�A 33A 33A �A��A=A=�A�MA7LA1A�A"hA�A�A�	A��A,�AߤA]�AeA�A�AW?A��A�8A"�AɆA�A �A�AoiA�nAOvAN�A��AߤA�EA�mAߤA1�A�A]dA+A5�AS�A�A%FAzxAOvA
�A
(�A	��A	��A	�	A�HA��A-wA�Ay>A�A��A�^A��AFtAںA1'A|�AW�A�A��A9�A�|A�KA��A4A��A+A a�@���@���@��$@���@�<6@���@�|@��3@���@�S�@��@�r�@���@�@�4@�	@�@�V@�#�@�u�@��/@�@�h
@�?@�S�@�{@�S&@줩@��D@�zx@��@���@�_@���@�h�@�_@�e@�N�@�`�@��.@�F@�9�@�H@�h@��K@���@�ff@�GE@�6�@�b@�F@�qv@�T�@��H@�@���@�l�@�L@��;@ߍP@�=�@ޓu@�1�@ݠ'@�h
@ۊ�@ڣ@�Q@���@�7�@�ϫ@�Vm@��B@�~@�[W@��@��#@�RT@қ�@�ϫ@�-w@В�@�4@��Q@Ϝ@�&@ε@�l"@���@��z@�,=@�u@͈f@�B�@���@��@�O�@ʹ�@�P�@�w�@�w2@�V@�K^@ŕ�@�E9@�C@��@đ�@�M�@��@×$@�\�@�7L@�@��0@�{J@�J#@��@���@���@�@�@��:@�*0@���@�$@���@�c@�?}@�%@���@�S�@�\�@��@��H@�Xy@�خ@���@�p�@�ȴ@�C-@��@�`B@��@��@��@�z�@�^5@���@�ƨ@�{J@��@�?�@��D@���@�hs@�C@��B@��<@�bN@�u@��#@���@� \@��9@�J�@�u@��>@�� @���@�:�@���@��\@�-@���@��	@�E9@���@�Q@��o@�{J@�A @���@�\�@�;�@���@���@��P@���@�Ft@��@���@��7@�n/@�X@��@���@�-@��)@��P@�G�@�	l@���@���@���@�S�@�"h@���@���@�  @�/�@��@��@� �@�	@�ݘ@��@���@��.@��'@���@�q@���@�M@�-�@�]d@�U2@���@���@�($@��@�J�@�$t@�C@�8@�;d@�q@���@���@�K^@���@�@�b@���@��@���@�|�@���@�y>@��@��m@���@�b�@��@��@�r�@�[�@�~�@�Ta@��@���@�~�@�Z�@�C�@�&@��@�C�@�`B@�g�@�~�@���@�/@��`@�xl@�h
@�e�@�N�@�]�@��Y@���@�c @�8�@��d@���@���@�s�@�hs@�Y�@�+�@���@��_@�c @��&@�e,@�;@��I@�l�@��@���@���@��7@�\�@�o@��p@�h�@�M@�!@��Z@���@�0�@���@���@���@�M�@��@��'@�J#@��@��@�
�@���@��@��C@�Z�@��@���@��!@��A@�`�@�C-@�"h@�4@��@��@��@��h@�F@�@@���@�z@�a|@�<�@��@�ϫ@��@@���@���@���@�K�@�q@�ں@��h@���@�]d@�:�@��@��9@�O@���@��_@�D�@�&�@�  @�@~d�@}��@}@|��@|Ft@{��@{\)@z��@z��@z8�@y��@y�"@x��@x�o@xb@w��@w�@v��@vJ�@uԕ@uF@u�@t�?@t��@t1'@s��@sU�@s�@rn�@q��@qzx@q0�@p��@p~@o�@o�
@o��@o��@o"�@nں@n��@n_�@nJ@m�T@m��@m(�@l�	@l�@lM@lM@k|�@kS@j��@jGE@i�9@hѷ@h]d@h7@g�}@g��@g��@g��@gj�@f�@f�@f�L@fn�@e�T@dی@dl"@c�@c�	@c�@b�@b�!@b�@b=q@b �@aO�@`�@`��@`c�@`9X@_��@_X�@^�@^ȴ@^xl@^�@]��@];@\�U@\N�@[��@[v`@Z�h@Z�@Z&�@Yw2@X��@X�@X�u@Xu�@XFt@X�@W�]@W��@W��@W>�@Vq�@Vu@U�X@UB�@T�9@TZ@T"h@S��@S�a@S��@S6z@R�@RZ�@R5?@R�@Q�>@Q�7@Q*0@P*�@O�@Nh
@M��@M�@L��@L��@L�@L�z@L6@K�&@Kqv@KiD@Jȴ@J�F@J;�@J�@I��@I��@I�@Is�@IN<@I@@H�@H�@G�@G�$@G��@G��@G,�@F�@FJ�@E��@D��@D~@C,�@B�@B�R@B��@B� @A�@ADg@@�E@@m�@@@?�r@?�W@?�@?�K@?o�@>0U@=�)@=�@=|@=N<@=*0@<�K@<�@<m�@<H@<@;� @;�0@;�q@;y�@;/�@;@;�@:�H@:C�@9��@9+�@9�@8�@8�E@8`�@7�;@7�@6��@6�1@6v�@6h
@6?@5��@5�T@5�S@5?}@5�@4��@4�/@4�z@4oi@4D�@4 �@4@3� @3n/@3)_@2�@2�@2�h@2��@2�R@2��@2��@2Ta@2?@2�@2	@1�T@1u�@1Dg@1�@0��@0U2@/��@/��@/@O@.��@.d�@-��@-Vm@-�@,��@,tT@,4n@+�]@+�0@+RT@*�"@*�1@*H�@*#:@)��@)m]@)G�@(�	@(�p@(�U@(��@(�@'��@'�$@&�B@&z@&h
@&.�@&!�@%��@%/@%@@$��@$�@$�v@$��@$��@$�D@$oi@$>B@$(�@$$@$M@#��@#�P@#qv@#]�@#>�@#!-@"~�@")�@!�)@!�9@!��@!��@!X@ �K@ �$@ |�@ !@��@�@y�@@O@�y@�!@��@Ov@�@�j@��@�@`B@Q�@IR@@@��@�/@Ĝ@��@��@�.@�o@C-@D�@��@��@��@>�@�@�@��@Q@0U@e@�@��@�C@�@�"@Vm@<6@+@�@�K@֡@�O@_@M@�r@˒@�@@�k@X�@�'@{�@^5@&�@��@�@��@��@F@<6@A @+�@�@�@��@�I@Xy@%�@1@G@�@�@ƨ@8@�@��@}V@@�@ �@@a�@��@�4@`�@4n@"h@~@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�:A�oA��A�4A��A��A�oA��A��A�{A�A�$A�+A�1A��A��A��A�kA�	A��A�A��A��A��A� �A�"4A�$A�#�A�%FA�'RA�)�A�!bA�kA�!-A�{A�A�{A�
=A�A�
�A�)�A��Aǹ$Aą�A��A��A�ĜA��MA��)A���A��DA�ŢA��A�m�A��A���A���A��A��tA��?A�{JA��vA���A��A�uA���A� �A��{A���A�'RA�~�A�ŢA��FA�kA���A�Y�A�\A���A���A�0�A�\A��A�,qA�d&A���A��A��'A�]�A�چA�+�A�̘A���A��\A���A���A�#�A��}A���A�"hA��A�A�A�A}�.Aw��At>�Ar:*An�AfJ�A^�AY%AVcAU+AT%ARYKAO�AN�AJԕAH��AG@�AD�AC�AC{AA:�A>��A=U2A=jA<B[A;�_A;>�A9\)A7oiA6?�A5�/A5��A5�)A6��A5xA3�+A3@OA2��A2d�A1�A0"hA,�A,|�A,�`A,�A*f�A+2�A+�AA+7A({�A&}VA&˒A&�A&y�A%��A%uA#��A#A"/�A"H�A"u�A">�A!��A!,�A 33A 33A �A��A=A=�A�MA7LA1A�A"hA�A�A�	A��A,�AߤA]�AeA�A�AW?A��A�8A"�AɆA�A �A�AoiA�nAOvAN�A��AߤA�EA�mAߤA1�A�A]dA+A5�AS�A�A%FAzxAOvA
�A
(�A	��A	��A	�	A�HA��A-wA�Ay>A�A��A�^A��AFtAںA1'A|�AW�A�A��A9�A�|A�KA��A4A��A+A a�@���@���@��$@���@�<6@���@�|@��3@���@�S�@��@�r�@���@�@�4@�	@�@�V@�#�@�u�@��/@�@�h
@�?@�S�@�{@�S&@줩@��D@�zx@��@���@�_@���@�h�@�_@�e@�N�@�`�@��.@�F@�9�@�H@�h@��K@���@�ff@�GE@�6�@�b@�F@�qv@�T�@��H@�@���@�l�@�L@��;@ߍP@�=�@ޓu@�1�@ݠ'@�h
@ۊ�@ڣ@�Q@���@�7�@�ϫ@�Vm@��B@�~@�[W@��@��#@�RT@қ�@�ϫ@�-w@В�@�4@��Q@Ϝ@�&@ε@�l"@���@��z@�,=@�u@͈f@�B�@���@��@�O�@ʹ�@�P�@�w�@�w2@�V@�K^@ŕ�@�E9@�C@��@đ�@�M�@��@×$@�\�@�7L@�@��0@�{J@�J#@��@���@���@�@�@��:@�*0@���@�$@���@�c@�?}@�%@���@�S�@�\�@��@��H@�Xy@�خ@���@�p�@�ȴ@�C-@��@�`B@��@��@��@�z�@�^5@���@�ƨ@�{J@��@�?�@��D@���@�hs@�C@��B@��<@�bN@�u@��#@���@� \@��9@�J�@�u@��>@�� @���@�:�@���@��\@�-@���@��	@�E9@���@�Q@��o@�{J@�A @���@�\�@�;�@���@���@��P@���@�Ft@��@���@��7@�n/@�X@��@���@�-@��)@��P@�G�@�	l@���@���@���@�S�@�"h@���@���@�  @�/�@��@��@� �@�	@�ݘ@��@���@��.@��'@���@�q@���@�M@�-�@�]d@�U2@���@���@�($@��@�J�@�$t@�C@�8@�;d@�q@���@���@�K^@���@�@�b@���@��@���@�|�@���@�y>@��@��m@���@�b�@��@��@�r�@�[�@�~�@�Ta@��@���@�~�@�Z�@�C�@�&@��@�C�@�`B@�g�@�~�@���@�/@��`@�xl@�h
@�e�@�N�@�]�@��Y@���@�c @�8�@��d@���@���@�s�@�hs@�Y�@�+�@���@��_@�c @��&@�e,@�;@��I@�l�@��@���@���@��7@�\�@�o@��p@�h�@�M@�!@��Z@���@�0�@���@���@���@�M�@��@��'@�J#@��@��@�
�@���@��@��C@�Z�@��@���@��!@��A@�`�@�C-@�"h@�4@��@��@��@��h@�F@�@@���@�z@�a|@�<�@��@�ϫ@��@@���@���@���@�K�@�q@�ں@��h@���@�]d@�:�@��@��9@�O@���@��_@�D�@�&�@�  @�@~d�@}��@}@|��@|Ft@{��@{\)@z��@z��@z8�@y��@y�"@x��@x�o@xb@w��@w�@v��@vJ�@uԕ@uF@u�@t�?@t��@t1'@s��@sU�@s�@rn�@q��@qzx@q0�@p��@p~@o�@o�
@o��@o��@o"�@nں@n��@n_�@nJ@m�T@m��@m(�@l�	@l�@lM@lM@k|�@kS@j��@jGE@i�9@hѷ@h]d@h7@g�}@g��@g��@g��@gj�@f�@f�@f�L@fn�@e�T@dی@dl"@c�@c�	@c�@b�@b�!@b�@b=q@b �@aO�@`�@`��@`c�@`9X@_��@_X�@^�@^ȴ@^xl@^�@]��@];@\�U@\N�@[��@[v`@Z�h@Z�@Z&�@Yw2@X��@X�@X�u@Xu�@XFt@X�@W�]@W��@W��@W>�@Vq�@Vu@U�X@UB�@T�9@TZ@T"h@S��@S�a@S��@S6z@R�@RZ�@R5?@R�@Q�>@Q�7@Q*0@P*�@O�@Nh
@M��@M�@L��@L��@L�@L�z@L6@K�&@Kqv@KiD@Jȴ@J�F@J;�@J�@I��@I��@I�@Is�@IN<@I@@H�@H�@G�@G�$@G��@G��@G,�@F�@FJ�@E��@D��@D~@C,�@B�@B�R@B��@B� @A�@ADg@@�E@@m�@@@?�r@?�W@?�@?�K@?o�@>0U@=�)@=�@=|@=N<@=*0@<�K@<�@<m�@<H@<@;� @;�0@;�q@;y�@;/�@;@;�@:�H@:C�@9��@9+�@9�@8�@8�E@8`�@7�;@7�@6��@6�1@6v�@6h
@6?@5��@5�T@5�S@5?}@5�@4��@4�/@4�z@4oi@4D�@4 �@4@3� @3n/@3)_@2�@2�@2�h@2��@2�R@2��@2��@2Ta@2?@2�@2	@1�T@1u�@1Dg@1�@0��@0U2@/��@/��@/@O@.��@.d�@-��@-Vm@-�@,��@,tT@,4n@+�]@+�0@+RT@*�"@*�1@*H�@*#:@)��@)m]@)G�@(�	@(�p@(�U@(��@(�@'��@'�$@&�B@&z@&h
@&.�@&!�@%��@%/@%@@$��@$�@$�v@$��@$��@$�D@$oi@$>B@$(�@$$@$M@#��@#�P@#qv@#]�@#>�@#!-@"~�@")�@!�)@!�9@!��@!��@!X@ �K@ �$@ |�@ !@��@�@y�@@O@�y@�!@��@Ov@�@�j@��@�@`B@Q�@IR@@@��@�/@Ĝ@��@��@�.@�o@C-@D�@��@��@��@>�@�@�@��@Q@0U@e@�@��@�C@�@�"@Vm@<6@+@�@�K@֡@�O@_@M@�r@˒@�@@�k@X�@�'@{�@^5@&�@��@�@��@��@F@<6@A @+�@�@�@��@�I@Xy@%�@1@G@�@�@ƨ@8@�@��@}V@@�@ �@@a�@��@�4@`�@4n@"h@~@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�"B
�B
�B
ΊB
ΊB
�VB
ΊB
�pB
ΊB
ΊB
�pB
ΥB
ΥB
�pB
�VB
�<B
�<B
�VB
�"B
�"B
�VB
�"B
�"B
�<B
�pB
�VB
ΥB
ΊB
ΥB
ΥB
͹B
͹B
��B
̈́B
�B
�PB
�dB
�B
�=B
�GB
�YB
�%B
}�B
�
B
��B
��B
�UB
��B
�RB
��B
�YB
�)B
�B
�-B
��B�B($B8RBN<BjKBr-B��B�B�OB�QBoB�B�B��BoB�BB��B�B�B��B��B�B��B��B�B}�BsBwfBo�Bi_Bc�B\xBMB1[B$&BQB
��B
ǔB
�lB
]dB
FB
1�B
!bB
  B	�B	�IB	��B	��B	��B	��B	`�B	8B	!-B	yB	:B	B	
�B	�B	�B	�B	YB	#B	;B	"�B	#B	*eB	6�B	A�B	T{B	_�B	z�B	|6B	|�B	u�B	lqB	vFB	��B	�B	��B	�qB	�*B	��B	��B	��B	�PB	��B	�wB	�wB	ɺB	҉B	خB	�*B	��B	��B	�[B	��B	�B	��B	�9B	��B	�BB	�B	�?B	�B	��B	��B	�]B	��B	�B	�B	��B	�HB	�B	�B	�B	چB	��B	ٴB	ѝB	�0B	ȀB	�?B	�xB	ӏB	��B	��B	�B	��B	�;B	ؓB	�B
 �B
�B	��B
�B
^B
(B
gB
�B

rB
�B
	�B
B
HB
 B
�B
�B
)B

=B

	B

rB
DB
�B
�B
�B
	B
�B
KB
�B
B
�B
�B
gB
	lB
_B
?B
�B
�B
�B
	lB
	�B
	�B
�B
�B
 �B
GB

�B
�B
�B
B
�B
�B
	�B
�B	��B	��B	�B	��B	ߤB	��B	οB	��B	յB	��B	�=B	�B	�KB	ٴB	��B	�$B	�QB	�_B	��B	��B	�vB	ΊB	��B	��B	�B	�B	�eB	�B	�2B	��B	�zB	��B	�tB	�:B	�5B	��B	��B	�jB	�B	�"B	ܒB	��B	ںB	��B	�vB	�B	�B	�B	��B	�B	��B	��B	�[B	�[B	�nB	��B	��B	��B	�%B	�B	��B	�|B	��B	�aB	�hB	�B	�MB	�B	��B	�B	�B	�|B	�hB	�hB	�3B	�B	�3B	�hB	�B	�3B	��B	�GB	�GB	��B	�;B	��B	��B	�oB	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	�eB	�B	��B	�B	��B	�-B	�B	�5B	�dB	��B	ݘB	�~B	ܬB	��B	ܬB	�)B	��B	��B	یB	��B	��B	ܬB	ܬB	��B	��B	�/B	��B	��B	ݲB	�B	��B	ޞB	�B	�pB	�B	�B	��B	�|B	��B	�-B	�-B	�NB	�TB	�B	�:B	�nB	��B	�B	�,B	�B	��B	�>B	�B	��B	�XB	�B	��B	�8B	��B	�mB	�B	�B	�B	�yB	��B	�eB	�B	�0B	�B	�B	�B	��B	��B	�OB	�B	�!B	�AB	�vB	�-B	�B	�B	�B	�B	��B	�B	�B	��B	�FB	�FB	��B	��B	��B	�LB	�8B	�rB	�$B	��B	�	B	�B	�^B	�>B	��B	�B	�JB	�B	�B	�dB	�6B	��B	��B	��B	��B
AB
UB
 4B	�cB
 �B
B
GB
�B
B
<B
�B
6B

XB
�B
�B
�B
"B
�B
�B
�B
)B
	�B

=B
)B

�B
�B
�B
B
�B
�B
�B
{B
�B
�B
zB
�B
MB
uB
�B
{B
�B
�B
'B
�B
B
{B
SB
�B

�B

�B
B
)B
�B
�B
B
�B
4B
B
[B
�B
?B
_B
�B
B
�B
�B
B
QB
�B
YB
�B
�B
�B
�B
7B
qB
xB
�B
]B
�B
�B
�B
/B
�B
�B
�B
�B
 vB
 �B
�B
 'B
 BB
 �B
!�B
!|B
!�B
"NB
#nB
#�B
$&B
$B
$�B
%B
$&B
$�B
%�B
&B
%�B
&2B
%�B
#�B
#�B
$@B
$tB
$�B
%FB
%,B
&�B
&�B
&�B
&�B
($B
)�B
)_B
)DB
)�B
*B
*0B
)DB
*�B
,"B
,B
+�B
+�B
+�B
,"B
,�B
-]B
-wB
-wB
-]B
-�B
-�B
.�B
.cB
.}B
/ B
/�B
/�B
/iB
0B
.�B
0�B
1AB
2GB
2�B
2GB
2-B
1�B
2-B
3B
4B
3�B
4�B
4�B
5?B
5%B
5ZB
5tB
5tB
6FB
6zB
6�B
7LB
7�B
7�B
8B
8B
88B
8RB
8�B
9rB
9�B
:B
:�B
;JB
;�B
;�B
<B
<�B
<PB
;�B
<B
<�B
<�B
<�B
=<B
=�B
=�B
>]B
>(B
>wB
>�B
?B
>�B
?cB
@ B
?�B
?�B
@�B
AUB
AUB
B'B
B�B
BuB
BB
A�B
B'B
B�B
BuB
B�B
B�B
B�B
A�B
A�B
B�B
C�B
C�B
DgB
C�B
D�B
DgB
D�B
EB
D3B
CaB
D�B
E�B
FYB
FYB
G�B
GB
G+B
G_B
G�B
H�B
H1B
G�B
G�B
IRB
I7B
H�B
HKB
G�B
IRB
IlB
I�B
I�B
J#B
J	B
J�B
I�B
KB
J�B
K^B
KDB
K�B
K�B
LJB
L�B
L�B
MB
L�B
LdB
MB
MB
M6B
M�B
MjB
L�B
MjB
L�B
M6B
L~B
LB
LJB
K�B
MB
N�B
PB
PB
O�B
P�B
Q�B
R�B
R B
Q�B
R�B
RoB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T{B
T�B
UMB
T�B
UB
U2B
UB
UMB
T{B
T{B
S�B
RTB
R�B
RTB
S[B
S�B
SuB
S�B
S[B
S@B
S�B
R�B
SuB
SB
S[B
T,B
UB
UgB
V�B
WYB
W�B
W�B
X�B
YeB
X�B
X�B
X�B
Y1B
YB
X�B
X�B
Y�B
ZB
YKB
YB
Z�B
[�B
[�B
[WB
[�B
[WB
[�B
[WB
[�B
\B
\B
\)B
[�B
\)B
\CB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
\�B
\�B
\xB
]IB
]~B
^B
_B
^�B
_�B
^�B
_B
_�B
_�B
`\B
_�B
`�B
`B
`vB
a�B
a�B
b�B
b�B
c�B
c�B
c�B
dB
cTB
c�B
dtB
c�B
d�B
d@B
dtB
d@B
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
f2B
fB
gRB
f�B
f�B
g�B
g�B
gB
f�B
g�B
gmB
g�B
h�B
g�B
h�B
i*B
iB
h�B
iDB
i*B
h�B
i*B
i�B
i�B
jKB
i�B
j0B
i�B
jKB
jB
jeB
j0B
jeB
jKB
j�B
kB
k�B
kkB
k�B
k�B
k�B
lB
l=B
lqB
l�B
m]B
m]B
mwB
m�B
m�B
m�B
nIB
n�B
n�B
ncB
n}B
n�B
oB
oOB
oOB
pB
pUB
p!B
p;B
p�B
p;B
p;B
p�B
poB
qB
p�B
rB
r|B
raB
sB
rGB
r�B
r�B
rGB
raB
r|B
shB
s�B
s�B
tTB
t�B
uB
vB
utB
u?B
u�B
vFB
u�B
v�B
wB
v�B
v�B
v�B
v�B
wfB
xB
x8B
x8B
x�B
x�B
x�B
x�B
x�B
y�B
y>B
x�B
yrB
y$B
y�B
yrB
y�B
zDB
y�B
z�B
z�B
y�B
zxB
zxB
z�B
zxB
{�B
{�B
|B
{�B
|PB
|6B
|PB
{�B
|B
|PB
|B
|B
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�"B
�B
�B
ΊB
ΊB
�VB
ΊB
�pB
ΊB
ΊB
�pB
ΥB
ΥB
�pB
�VB
�<B
�<B
�VB
�"B
�"B
�VB
�"B
�"B
�<B
�pB
�VB
ΥB
ΊB
ΥB
ΥB
͹B
͹B
��B
̈́B
�B
�PB
�dB
�B
�=B
�GB
�YB
�%B
}�B
�
B
��B
��B
�UB
��B
�RB
��B
�YB
�)B
�B
�-B
��B�B($B8RBN<BjKBr-B��B�B�OB�QBoB�B�B��BoB�BB��B�B�B��B��B�B��B��B�B}�BsBwfBo�Bi_Bc�B\xBMB1[B$&BQB
��B
ǔB
�lB
]dB
FB
1�B
!bB
  B	�B	�IB	��B	��B	��B	��B	`�B	8B	!-B	yB	:B	B	
�B	�B	�B	�B	YB	#B	;B	"�B	#B	*eB	6�B	A�B	T{B	_�B	z�B	|6B	|�B	u�B	lqB	vFB	��B	�B	��B	�qB	�*B	��B	��B	��B	�PB	��B	�wB	�wB	ɺB	҉B	خB	�*B	��B	��B	�[B	��B	�B	��B	�9B	��B	�BB	�B	�?B	�B	��B	��B	�]B	��B	�B	�B	��B	�HB	�B	�B	�B	چB	��B	ٴB	ѝB	�0B	ȀB	�?B	�xB	ӏB	��B	��B	�B	��B	�;B	ؓB	�B
 �B
�B	��B
�B
^B
(B
gB
�B

rB
�B
	�B
B
HB
 B
�B
�B
)B

=B

	B

rB
DB
�B
�B
�B
	B
�B
KB
�B
B
�B
�B
gB
	lB
_B
?B
�B
�B
�B
	lB
	�B
	�B
�B
�B
 �B
GB

�B
�B
�B
B
�B
�B
	�B
�B	��B	��B	�B	��B	ߤB	��B	οB	��B	յB	��B	�=B	�B	�KB	ٴB	��B	�$B	�QB	�_B	��B	��B	�vB	ΊB	��B	��B	�B	�B	�eB	�B	�2B	��B	�zB	��B	�tB	�:B	�5B	��B	��B	�jB	�B	�"B	ܒB	��B	ںB	��B	�vB	�B	�B	�B	��B	�B	��B	��B	�[B	�[B	�nB	��B	��B	��B	�%B	�B	��B	�|B	��B	�aB	�hB	�B	�MB	�B	��B	�B	�B	�|B	�hB	�hB	�3B	�B	�3B	�hB	�B	�3B	��B	�GB	�GB	��B	�;B	��B	��B	�oB	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	�eB	�B	��B	�B	��B	�-B	�B	�5B	�dB	��B	ݘB	�~B	ܬB	��B	ܬB	�)B	��B	��B	یB	��B	��B	ܬB	ܬB	��B	��B	�/B	��B	��B	ݲB	�B	��B	ޞB	�B	�pB	�B	�B	��B	�|B	��B	�-B	�-B	�NB	�TB	�B	�:B	�nB	��B	�B	�,B	�B	��B	�>B	�B	��B	�XB	�B	��B	�8B	��B	�mB	�B	�B	�B	�yB	��B	�eB	�B	�0B	�B	�B	�B	��B	��B	�OB	�B	�!B	�AB	�vB	�-B	�B	�B	�B	�B	��B	�B	�B	��B	�FB	�FB	��B	��B	��B	�LB	�8B	�rB	�$B	��B	�	B	�B	�^B	�>B	��B	�B	�JB	�B	�B	�dB	�6B	��B	��B	��B	��B
AB
UB
 4B	�cB
 �B
B
GB
�B
B
<B
�B
6B

XB
�B
�B
�B
"B
�B
�B
�B
)B
	�B

=B
)B

�B
�B
�B
B
�B
�B
�B
{B
�B
�B
zB
�B
MB
uB
�B
{B
�B
�B
'B
�B
B
{B
SB
�B

�B

�B
B
)B
�B
�B
B
�B
4B
B
[B
�B
?B
_B
�B
B
�B
�B
B
QB
�B
YB
�B
�B
�B
�B
7B
qB
xB
�B
]B
�B
�B
�B
/B
�B
�B
�B
�B
 vB
 �B
�B
 'B
 BB
 �B
!�B
!|B
!�B
"NB
#nB
#�B
$&B
$B
$�B
%B
$&B
$�B
%�B
&B
%�B
&2B
%�B
#�B
#�B
$@B
$tB
$�B
%FB
%,B
&�B
&�B
&�B
&�B
($B
)�B
)_B
)DB
)�B
*B
*0B
)DB
*�B
,"B
,B
+�B
+�B
+�B
,"B
,�B
-]B
-wB
-wB
-]B
-�B
-�B
.�B
.cB
.}B
/ B
/�B
/�B
/iB
0B
.�B
0�B
1AB
2GB
2�B
2GB
2-B
1�B
2-B
3B
4B
3�B
4�B
4�B
5?B
5%B
5ZB
5tB
5tB
6FB
6zB
6�B
7LB
7�B
7�B
8B
8B
88B
8RB
8�B
9rB
9�B
:B
:�B
;JB
;�B
;�B
<B
<�B
<PB
;�B
<B
<�B
<�B
<�B
=<B
=�B
=�B
>]B
>(B
>wB
>�B
?B
>�B
?cB
@ B
?�B
?�B
@�B
AUB
AUB
B'B
B�B
BuB
BB
A�B
B'B
B�B
BuB
B�B
B�B
B�B
A�B
A�B
B�B
C�B
C�B
DgB
C�B
D�B
DgB
D�B
EB
D3B
CaB
D�B
E�B
FYB
FYB
G�B
GB
G+B
G_B
G�B
H�B
H1B
G�B
G�B
IRB
I7B
H�B
HKB
G�B
IRB
IlB
I�B
I�B
J#B
J	B
J�B
I�B
KB
J�B
K^B
KDB
K�B
K�B
LJB
L�B
L�B
MB
L�B
LdB
MB
MB
M6B
M�B
MjB
L�B
MjB
L�B
M6B
L~B
LB
LJB
K�B
MB
N�B
PB
PB
O�B
P�B
Q�B
R�B
R B
Q�B
R�B
RoB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T{B
T�B
UMB
T�B
UB
U2B
UB
UMB
T{B
T{B
S�B
RTB
R�B
RTB
S[B
S�B
SuB
S�B
S[B
S@B
S�B
R�B
SuB
SB
S[B
T,B
UB
UgB
V�B
WYB
W�B
W�B
X�B
YeB
X�B
X�B
X�B
Y1B
YB
X�B
X�B
Y�B
ZB
YKB
YB
Z�B
[�B
[�B
[WB
[�B
[WB
[�B
[WB
[�B
\B
\B
\)B
[�B
\)B
\CB
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]IB
\�B
\�B
\xB
]IB
]~B
^B
_B
^�B
_�B
^�B
_B
_�B
_�B
`\B
_�B
`�B
`B
`vB
a�B
a�B
b�B
b�B
c�B
c�B
c�B
dB
cTB
c�B
dtB
c�B
d�B
d@B
dtB
d@B
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
f2B
fB
gRB
f�B
f�B
g�B
g�B
gB
f�B
g�B
gmB
g�B
h�B
g�B
h�B
i*B
iB
h�B
iDB
i*B
h�B
i*B
i�B
i�B
jKB
i�B
j0B
i�B
jKB
jB
jeB
j0B
jeB
jKB
j�B
kB
k�B
kkB
k�B
k�B
k�B
lB
l=B
lqB
l�B
m]B
m]B
mwB
m�B
m�B
m�B
nIB
n�B
n�B
ncB
n}B
n�B
oB
oOB
oOB
pB
pUB
p!B
p;B
p�B
p;B
p;B
p�B
poB
qB
p�B
rB
r|B
raB
sB
rGB
r�B
r�B
rGB
raB
r|B
shB
s�B
s�B
tTB
t�B
uB
vB
utB
u?B
u�B
vFB
u�B
v�B
wB
v�B
v�B
v�B
v�B
wfB
xB
x8B
x8B
x�B
x�B
x�B
x�B
x�B
y�B
y>B
x�B
yrB
y$B
y�B
yrB
y�B
zDB
y�B
z�B
z�B
y�B
zxB
zxB
z�B
zxB
{�B
{�B
|B
{�B
|PB
|6B
|PB
{�B
|B
|PB
|B
|B
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104920  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173833  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173833  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173833                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023841  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023841  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                