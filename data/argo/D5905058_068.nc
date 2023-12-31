CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-17T03:35:18Z creation;2018-06-17T03:35:20Z conversion to V3.1;2019-12-23T06:20:09Z update;     
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
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180617033518  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA  I2_0675_068                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�j�`�� 1   @�j�`� @8���e���c&q�i�C1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A(��AH��Ah��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B
(�B(�B(�B"(�B*(�B2(�B:(�BB(�BJ(�BR(�BZ(�Bb(�Bj(�Br(�Bz(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C �=C�=C�=C�=C�=C
�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C �=C"�=C$�=C&�=C(�=C*�=C,�=C.�=C0�=C2�=C4�=C6�=C8�=C:�=C<�=C>�=C@�=CB�=CD�=CF�=CH�=CJ�=CL�=CN�=CP�=CR�=CT�=CV�=CX�=CZ�=C\�=C^�=C`�=Cb�=Cd�=Cf�=Ch�=Cj�=Cl�=Cn�=Cp�=Cr�=Ct�=Cv�=Cx�=Cz�=C|�=C~�=C�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�8RC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�EC�ED "�D ��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D	"�D	��D
"�D
��D"�D��D"�D��D"�D��D"�D�)D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D"�D��D "�D ��D!"�D!��D""�D"��D#"�D#��D$"�D$��D%"�D%��D&"�D&��D'"�D'��D("�D(��D)"�D)��D*"�D*��D+"�D+��D,"�D,��D-"�D-��D."�D.��D/"�D/��D0"�D0��D1"�D1��D2"�D2��D3"�D3��D4"�D4��D5"�D5��D6"�D6��D7"�D7��D8"�D8��D9"�D9��D:"�D:��D;"�D;��D<"�D<��D="�D=��D>"�D>��D?"�D?��D@"�D@��DA"�DA��DB"�DB��DC"�DC��DD"�DD��DE"�DE��DF"�DF��DG"�DG��DH"�DH��DI"�DI��DJ"�DJ��DK"�DK��DL"�DL��DM"�DM��DN"�DN��DO"�DO��DP"�DP��DQ"�DQ��DR"�DR��DS"�DS��DT"�DT��DU"�DU��DV"�DV��DW"�DW��DX"�DX��DY"�DY��DZ"�DZ��D["�D[�)D\"�D\��D]"�D]��D^"�D^��D_"�D_��D`"�D`��Da"�Da��Db"�Db��Dc"�Dc��Dd"�Dd��De"�De��Df"�Df��Dg"�Dg��Dh"�Dh��Di"�Di��Dj"�Dj��Dk"�Dk��Dl"�Dl��Dm"�Dm��Dn"�Dn��Do"�Do��Dp"�Dp��Dq"�Dq��Dr"�Dr��Ds"�Ds��Dt"�Dt��Du"�Du��Dv"�Dv��Dw"�Dw��Dx"�Dx��Dy"�Dy��Dz"�Dz��D{"�D{��D|"�D|��D}"�D}��D~"�D~��D"�D��D�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�T{D��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�ND��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHDHD��HD�HD�QHDÑHD��HD�HD�QHDđHD��HD�HD�QHDőHD��HD�HD�QHDƑHD��HD�HD�QHDǑHD��HD�HD�QHDȑHD��HD�HD�QHDɑHD��HD�HD�QHDʑHD��HD�HD�QHDˑHD��HD�HD�QHD̑HD��HD�HD�QHD͑HD��HD�HD�QHDΑHD��HD�HD�QHDϑHD��HD�HD�QHDБHD��HD�HD�QHDёHD��HD�HD�QHDґHD��HD�HD�QHDӑHD��HD�HD�QHDԑHD��HD�HD�QHDՑHD��HD�HD�QHD֑HD��HD�HD�QHDבHD��HD�HD�QHDؑHD��HD�HD�QHDّHD��HD�HD�QHDڑHD��HD�HD�QHDۑHD��HD�HD�QHDܑHD��HD�HD�QHDݑHD��HD�HD�QHDޑHD��HD�HD�QHDߑHD��HD�HD�QHD��HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD�HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�QHD��HD��HD�HD�T{D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�7LA�`BA�$�A���A�Q�A�33A��A���A���A�`BA���A�5?A��RA��A�(�A�z�A���A�G�A�n�A���A�t�A�XA�$�A��/A�XA��/A�r�A��!A�I�A��RA��jA�A�x�A���A�hsA�/A��7A�XA�  A��uA��uA���A�hsA�bA�dZA���A��A��A��7A�A�\)A���A�%A�^5A�n�A�
=A�ĜA���A�ƨA���A��A���A��A�ƨA��A��uA���A��9A�r�A��A��!A��!A�JA�;dA��A��
A�  A��yA��#A��jA��mA�VA���A��A�`BA�hsA��/A��jA�bNA�7LA�n�A�1'A�`BA��A}�Azn�Axv�Aw�TAw�Av�RAs;dAq�wAp�An��Am��Akx�AjZAil�AgAe�;Ad�AbffAa`BA`ZA]��A[�AZr�AY�AX{AU�ATr�AS��AR�!AQ�AQ7LAPQ�AO�AO/AN-AMt�ALA�AJ~�AI|�AHr�AG�7AG
=AF1AEl�AD�AD�+ADQ�AC�AB�DAAA@r�A@�A?t�A>ZA<�+A:��A8�A7oA6{A4��A3��A2��A1dZA1
=A0ZA/?}A.-A-l�A,�jA+�A+A*�/A(�+A(bA'�;A&ȴA%�A%�PA%VA$(�A#�hA"��A!�A ��A��A�AE�AC�AjA��A�A�Ax�A�HAA�uA��A�A��Ap�A33A�jA��AA�AA�A�hA%A��A�A�A�\AƨA
�`A	��A	C�A�/AM�AƨA�A��An�A\)A�A�Ar�A�A�-AV@�C�@�=q@��@�(�@�K�@���@��@��9@�l�@��@���@��@��^@�F@�+@�ȴ@���@�I�@��@�{@�`B@�w@�^@�%@�
=@ݡ�@���@��@�5?@��@�Q�@���@�Z@ҧ�@�?}@�ƨ@ΰ!@�G�@̃@˥�@�@�p�@ȣ�@�l�@ư!@�5?@Ų-@�x�@�?}@���@�
=@���@���@�p�@���@�1@�ƨ@���@�ȴ@��@�ff@���@�O�@�V@��m@��@��!@��+@�p�@��@���@��@���@���@��@���@��w@��@���@�hs@�X@��9@�Z@�A�@��m@�t�@�5?@���@��@�O�@�%@��
@��+@�x�@�Ĝ@�z�@�j@�ƨ@��F@�K�@�ȴ@�n�@���@�/@��j@��u@�j@�  @���@�33@�\)@���@��@�dZ@��!@��+@�~�@�J@��@�`B@�?}@��@���@�1'@��P@�K�@�
=@��@��R@���@�^5@�V@�^5@�V@��#@���@��h@��7@��@�hs@�O�@�G�@��@�Ĝ@�r�@�1'@�  @��F@�|�@�
=@���@�ȴ@���@��+@�-@��T@��-@��h@��^@��h@�x�@�Z@�b@� �@�  @���@�;d@�"�@���@�~�@���@��T@���@�1'@�V@���@��@��@���@��+@���@�7L@���@���@�r�@��@�Ĝ@��@�Ĝ@��D@�(�@��
@��@��P@���@��@��w@�+@��R@���@��@��+@�{@�p�@��@��@���@��u@�j@���@�j@�Z@�I�@�9X@��@�b@��w@���@�  @��@�l�@���@��\@���@��H@�t�@��@��@��@��@�dZ@�C�@�o@���@�E�@��@�M�@�n�@�v�@�ff@�$�@���@�p�@�p�@��9@��j@��j@���@��u@��@�bN@�1'@���@���@�dZ@�+@�o@��@���@��\@�n�@�E�@�5?@�5?@�-@�$�@�@�@��h@�hs@�&�@��@��9@���@�I�@�  @��w@�l�@�"�@��@���@���@�~�@�=q@��T@���@�x�@�hs@�O�@�/@���@���@��@�Z@�9X@�9X@�1'@�b@��@l�@~��@~5?@}�T@}@}p�@}/@|��@|��@|j@|(�@{�
@{��@{dZ@{o@zn�@zJ@yhs@xbN@w|�@u�@uV@s�
@s33@s"�@rn�@q�^@qhs@qG�@q��@q��@q��@qx�@qx�@q7L@q�7@qhs@o|�@n5?@nV@nȴ@n�y@lI�@i��@i%@hĜ@hbN@h��@ix�@jJ@j=q@i�#@h��@h��@hbN@h �@g�@g�@f��@fE�@e��@e�h@e/@d�@d�D@c�m@cdZ@c"�@co@b�@b�H@bM�@a�7@a�@`Ĝ@_\)@^��@^5?@^@]��@]V@\(�@\(�@\(�@\�@[ƨ@[S�@Z�H@ZJ@Y�@Y�@Y�@Y�#@Y��@Y�^@Y7L@X�9@X�@XQ�@X �@W��@W�P@V��@V�@V�R@V��@VV@V5?@V@U��@U�h@U`B@UO�@UV@T�@Tj@T9X@S�F@S�@SdZ@S33@R�H@R�H@R��@R-@Q��@Q�@P�`@P��@P�9@PA�@O�P@Ol�@Ol�@O\)@O;d@O
=@Nv�@N$�@N@M�@M�@M��@M@M@M�-@Mp�@M/@M�@L��@L��@L�@Lz�@Lj@L�@Kƨ@K�@KC�@Ko@J�@J�!@J^5@I�@Ix�@I7L@H��@G�w@G
=@F�y@E��@EV@D�j@DI�@D1@D1@Cƨ@CS�@C33@Co@B�H@B��@B~�@B^5@B-@B�@A��@A�^@A��@A�7@A&�@@Ĝ@@�9@@��@@�@@bN@@Q�@@A�@@1'@@ �@?�;@?�@?��@?l�@?+@>��@>�+@>V@>$�@>@=�h@=V@<�@<�j@<�D@<I�@<1@;�m@;��@;33@:�@:-@9�#@9x�@9hs@9hs@97L@8��@8��@8�u@8�u@8 �@7�w@7��@7�P@7|�@7K�@6�y@6�R@6��@6$�@5�@5O�@5?}@4��@4�D@3��@3�@3o@2�H@2��@2��@2M�@1�@1X@1�@0��@0�9@0�u@0�@0r�@0bN@/�@/�P@/K�@/�@.�R@.�+@.{@-��@-��@-�@-O�@-V@,��@,1@+��@+t�@+"�@*~�@*-@)��@)�@)�@)��@)��@)X@)7L@)�@(��@(�u@(r�@(Q�@(  @'�@'��@'+@&��@&�@&�@&ȴ@&��@&V@&{@&@%�T@%�-@%��@%��@%�h@%�@%`B@%?}@%�@$�/@$�j@$z�@$j@$9X@$1@#��@#C�@"��@"~�@"=q@!��@!hs@ �9@ r�@ �@ �u@ �u@ �@ r�@ bN@ Q�@ A�@  �@�@�;@�;@�;@��@�w@�w@�@�P@|�@l�@;d@�@�+@V@E�@{@�@�@`B@?}@V@�j@j@(�@�@��@��@t�@33@�H@��@�!@�!@�\@~�@M�@�@��@��@�7@hs@X@%@�u@Q�@A�@ �@b@  @|�@;d@+@�@�y@�R@��@��@ff@E�@5?@{@�T@��@��@p�@�@�/@��@��@�@z�@j@I�@(�@��@�m@��@C�@o@�H@��@��@�!@��@��@�\@~�@M�@-@J@�#@�7@X@�@��@��@�u@�@r�@1'@  @��@�@|�@\)@K�@+@�@�y@�y@ȴ@�R@��@�+@ff@E�@$�@{@�T@�-@��@�@p�@O�@��@�/@�j@�D@�D@z�@z�@z�@j@9X@1@ƨ@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�7LA�`BA�$�A���A�Q�A�33A��A���A���A�`BA���A�5?A��RA��A�(�A�z�A���A�G�A�n�A���A�t�A�XA�$�A��/A�XA��/A�r�A��!A�I�A��RA��jA�A�x�A���A�hsA�/A��7A�XA�  A��uA��uA���A�hsA�bA�dZA���A��A��A��7A�A�\)A���A�%A�^5A�n�A�
=A�ĜA���A�ƨA���A��A���A��A�ƨA��A��uA���A��9A�r�A��A��!A��!A�JA�;dA��A��
A�  A��yA��#A��jA��mA�VA���A��A�`BA�hsA��/A��jA�bNA�7LA�n�A�1'A�`BA��A}�Azn�Axv�Aw�TAw�Av�RAs;dAq�wAp�An��Am��Akx�AjZAil�AgAe�;Ad�AbffAa`BA`ZA]��A[�AZr�AY�AX{AU�ATr�AS��AR�!AQ�AQ7LAPQ�AO�AO/AN-AMt�ALA�AJ~�AI|�AHr�AG�7AG
=AF1AEl�AD�AD�+ADQ�AC�AB�DAAA@r�A@�A?t�A>ZA<�+A:��A8�A7oA6{A4��A3��A2��A1dZA1
=A0ZA/?}A.-A-l�A,�jA+�A+A*�/A(�+A(bA'�;A&ȴA%�A%�PA%VA$(�A#�hA"��A!�A ��A��A�AE�AC�AjA��A�A�Ax�A�HAA�uA��A�A��Ap�A33A�jA��AA�AA�A�hA%A��A�A�A�\AƨA
�`A	��A	C�A�/AM�AƨA�A��An�A\)A�A�Ar�A�A�-AV@�C�@�=q@��@�(�@�K�@���@��@��9@�l�@��@���@��@��^@�F@�+@�ȴ@���@�I�@��@�{@�`B@�w@�^@�%@�
=@ݡ�@���@��@�5?@��@�Q�@���@�Z@ҧ�@�?}@�ƨ@ΰ!@�G�@̃@˥�@�@�p�@ȣ�@�l�@ư!@�5?@Ų-@�x�@�?}@���@�
=@���@���@�p�@���@�1@�ƨ@���@�ȴ@��@�ff@���@�O�@�V@��m@��@��!@��+@�p�@��@���@��@���@���@��@���@��w@��@���@�hs@�X@��9@�Z@�A�@��m@�t�@�5?@���@��@�O�@�%@��
@��+@�x�@�Ĝ@�z�@�j@�ƨ@��F@�K�@�ȴ@�n�@���@�/@��j@��u@�j@�  @���@�33@�\)@���@��@�dZ@��!@��+@�~�@�J@��@�`B@�?}@��@���@�1'@��P@�K�@�
=@��@��R@���@�^5@�V@�^5@�V@��#@���@��h@��7@��@�hs@�O�@�G�@��@�Ĝ@�r�@�1'@�  @��F@�|�@�
=@���@�ȴ@���@��+@�-@��T@��-@��h@��^@��h@�x�@�Z@�b@� �@�  @���@�;d@�"�@���@�~�@���@��T@���@�1'@�V@���@��@��@���@��+@���@�7L@���@���@�r�@��@�Ĝ@��@�Ĝ@��D@�(�@��
@��@��P@���@��@��w@�+@��R@���@��@��+@�{@�p�@��@��@���@��u@�j@���@�j@�Z@�I�@�9X@��@�b@��w@���@�  @��@�l�@���@��\@���@��H@�t�@��@��@��@��@�dZ@�C�@�o@���@�E�@��@�M�@�n�@�v�@�ff@�$�@���@�p�@�p�@��9@��j@��j@���@��u@��@�bN@�1'@���@���@�dZ@�+@�o@��@���@��\@�n�@�E�@�5?@�5?@�-@�$�@�@�@��h@�hs@�&�@��@��9@���@�I�@�  @��w@�l�@�"�@��@���@���@�~�@�=q@��T@���@�x�@�hs@�O�@�/@���@���@��@�Z@�9X@�9X@�1'@�b@��@l�@~��@~5?@}�T@}@}p�@}/@|��@|��@|j@|(�@{�
@{��@{dZ@{o@zn�@zJ@yhs@xbN@w|�@u�@uV@s�
@s33@s"�@rn�@q�^@qhs@qG�@q��@q��@q��@qx�@qx�@q7L@q�7@qhs@o|�@n5?@nV@nȴ@n�y@lI�@i��@i%@hĜ@hbN@h��@ix�@jJ@j=q@i�#@h��@h��@hbN@h �@g�@g�@f��@fE�@e��@e�h@e/@d�@d�D@c�m@cdZ@c"�@co@b�@b�H@bM�@a�7@a�@`Ĝ@_\)@^��@^5?@^@]��@]V@\(�@\(�@\(�@\�@[ƨ@[S�@Z�H@ZJ@Y�@Y�@Y�@Y�#@Y��@Y�^@Y7L@X�9@X�@XQ�@X �@W��@W�P@V��@V�@V�R@V��@VV@V5?@V@U��@U�h@U`B@UO�@UV@T�@Tj@T9X@S�F@S�@SdZ@S33@R�H@R�H@R��@R-@Q��@Q�@P�`@P��@P�9@PA�@O�P@Ol�@Ol�@O\)@O;d@O
=@Nv�@N$�@N@M�@M�@M��@M@M@M�-@Mp�@M/@M�@L��@L��@L�@Lz�@Lj@L�@Kƨ@K�@KC�@Ko@J�@J�!@J^5@I�@Ix�@I7L@H��@G�w@G
=@F�y@E��@EV@D�j@DI�@D1@D1@Cƨ@CS�@C33@Co@B�H@B��@B~�@B^5@B-@B�@A��@A�^@A��@A�7@A&�@@Ĝ@@�9@@��@@�@@bN@@Q�@@A�@@1'@@ �@?�;@?�@?��@?l�@?+@>��@>�+@>V@>$�@>@=�h@=V@<�@<�j@<�D@<I�@<1@;�m@;��@;33@:�@:-@9�#@9x�@9hs@9hs@97L@8��@8��@8�u@8�u@8 �@7�w@7��@7�P@7|�@7K�@6�y@6�R@6��@6$�@5�@5O�@5?}@4��@4�D@3��@3�@3o@2�H@2��@2��@2M�@1�@1X@1�@0��@0�9@0�u@0�@0r�@0bN@/�@/�P@/K�@/�@.�R@.�+@.{@-��@-��@-�@-O�@-V@,��@,1@+��@+t�@+"�@*~�@*-@)��@)�@)�@)��@)��@)X@)7L@)�@(��@(�u@(r�@(Q�@(  @'�@'��@'+@&��@&�@&�@&ȴ@&��@&V@&{@&@%�T@%�-@%��@%��@%�h@%�@%`B@%?}@%�@$�/@$�j@$z�@$j@$9X@$1@#��@#C�@"��@"~�@"=q@!��@!hs@ �9@ r�@ �@ �u@ �u@ �@ r�@ bN@ Q�@ A�@  �@�@�;@�;@�;@��@�w@�w@�@�P@|�@l�@;d@�@�+@V@E�@{@�@�@`B@?}@V@�j@j@(�@�@��@��@t�@33@�H@��@�!@�!@�\@~�@M�@�@��@��@�7@hs@X@%@�u@Q�@A�@ �@b@  @|�@;d@+@�@�y@�R@��@��@ff@E�@5?@{@�T@��@��@p�@�@�/@��@��@�@z�@j@I�@(�@��@�m@��@C�@o@�H@��@��@�!@��@��@�\@~�@M�@-@J@�#@�7@X@�@��@��@�u@�@r�@1'@  @��@�@|�@\)@K�@+@�@�y@�y@ȴ@�R@��@�+@ff@E�@$�@{@�T@�-@��@�@p�@O�@��@�/@�j@�D@�D@z�@z�@z�@j@9X@1@ƨ@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BE�B_;B�B�%B�B�B�B� B~�B|�Bw�Bq�Bo�Bo�By�By�Bz�B�B�7B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB�=B�=B�bB��B��B��B�bB�+B�B|�Bs�BffBaHBcTBffB`BBYBR�BH�B>wB5?B2-B1'B!�BbBB  B��B�B�BoBBBB��B�B�B�B�HB��B�^B��B�1Br�B_;BJ�B;dB.B&�B �B�BbB
��B
��B
�XB
��B
�%B
s�B
cTB
\)B
R�B
;dB
-B
'�B
#�B
�B
1B	��B	�B	�NB	�)B	��B	ŢB	�wB	�3B	��B	��B	�oB	�B	z�B	l�B	W
B	R�B	L�B	B�B	2-B	,B	'�B	!�B	�B	�B	�B	bB	VB		7B	B��B�B�B�yB�`B�NB�HB�)B�B�5B�HB�HB�B��B��B��B��BƨB�dB�-B��B��B��B�uB�VB�JB�1B�B�B}�Bx�By�Bv�Bo�Bl�BgmB^5BXBVBQ�BM�BM�BM�BL�BO�BM�BM�BK�BI�BH�BH�BI�BD�BA�B@�B@�B>wB>wB<jB:^B7LB33B0!B/B,B-B/B.B.B.B/B-B,B-B,B)�B)�B(�B+B'�B'�B'�B'�B&�B%�B%�B%�B#�B$�B"�B!�B!�B!�B!�B �B!�B �B�B�B�B�B�B�B�B�B�B!�B!�B!�B#�B"�B"�B#�B"�B$�B$�B$�B%�B&�B'�B'�B(�B(�B(�B+B+B+B+B,B,B-B.B/B0!B1'B2-B49B5?B6FB8RB8RB8RB8RB?}B@�B@�B@�BC�BB�BC�BE�BE�BK�BK�BO�BP�BO�BT�BYBYBYB]/B]/B^5BaHBdZBhsBhsBy�B{�By�Bz�Bz�B�B�B�B�B�%B�7B�DB�DB�bB�{B�{B�uB�oB��B��B��B��B��B��B��B��B��B��B�B�9B�?B�FB�RB�jB�}BŢB��B��B��B�B�B�B�B�B�B�B�B�#B�5B�HB�TB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	JB	VB	VB	\B	hB	hB	uB	�B	�B	�B	�B	�B	!�B	(�B	.B	49B	;dB	=qB	A�B	D�B	H�B	J�B	O�B	T�B	VB	YB	W
B	W
B	XB	_;B	`BB	dZB	cTB	aHB	aHB	cTB	e`B	ffB	jB	m�B	q�B	v�B	x�B	z�B	|�B	�B	�B	�B	�1B	�JB	�\B	�uB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�3B	�9B	�FB	�RB	�jB	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ŢB	ŢB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
  B
B
B
1B
B
B
B
B
B
%B

=B
PB
\B
\B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
hB
hB
hB
hB
hB
hB
uB
uB
oB
oB
hB
hB
hB
hB
hB
hB
hB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
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
)�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
/B
/B
/B
0!B
0!B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
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
8RB
8RB
9XB
9XB
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
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
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
?}B
@�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
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
E�B
E�B
E�B
E�B
E�B
F�B
F�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
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
bNB
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
cTB
dZB
dZB
dZB
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
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
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
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BEmB_B��B��B��B��B��B�B~�B|�Bw�BqvBo�Bo�By�By�Bz�B��B�B�4B�YB�_B�_B�eB�qB��B��B��B��B��B��B��B��B�kB�0B�	B�	B�.B�YB�YB�MB�.B��B��B|�Bs�Bf2BaBc Bf2B`BX�BR�BH�B>BB5B1�B0�B!�B.B�B��B��B�vB�B:B�B�B�B��B�|B�B�KB�BҽB�*B�kB��Br|B_BJ�B;0B-�B&�B �B~B.B
��B
ҽB
�$B
��B
��B
s�B
c B
[�B
R�B
;0B
,�B
'�B
#�B
�B
�B	��B	�oB	�B	��B	ΊB	�mB	�BB	��B	��B	�xB	�:B	��B	z�B	lWB	V�B	R�B	L�B	B[B	1�B	+�B	'�B	!�B	~B	?B	MB	.B	B		B	�B��B�vB�]B�DB�B��B�B��B��B��B�B�B��B��BϑBϫB̘B�tB�0B��B��B��B�eB�@B�B�B��B��B��B}�Bx�By�BvzBoiBlWBgB^BW�BU�BQ�BM�BM�BM�BL~BO�BM�BM�BK�BI�BHfBHfBIlBDgBAUB@OB@OB>BB>BB<6B:B7B2�B/�B.�B+�B,�B.�B-�B-�B-�B.�B,�B+�B,�B+�B)�B)�B(�B*�B'�B'�B'�B'�B&�B%�B%�B%�B#�B$�B"�B!�B!|B!�B!�B �B!�B vB�B�B�BjB~B~BdB�B�B!�B!|B!�B#�B"�B"�B#�B"�B$�B$�B$�B%�B&�B'�B'�B(�B(�B(�B*�B*�B*�B*�B+�B+�B,�B-�B.�B/�B0�B1�B4B4�B5�B8B8B8B8B?.B@4B@4B@4BCaBBABCGBESBEmBKxBKxBO�BP�BO�BT�BX�BX�BX�B\�B\�B^BaBdBh$Bh$By�B{�By�Bz�Bz�B��B��B��B��B��B��B��B��B�.B�FB�,B�&B�:B�2B�?B�YB�YB�qB�jB�pB�vB��B��B��B�B�B��B�B�6B�.B�SB˒B̈́BԯB��B��B��B��B��B��B��B��B��B�B�B�B�2B�*B�0B�QB�=B�]B�cB�vB�nB�tB�tB�tB�tB�tB��B��B��B��B	�B	B	B	B	B	B	B	&B	EB	QB	dB	pB	pB	!|B	(�B	-�B	3�B	;0B	=<B	AUB	DMB	HfB	JrB	O�B	T�B	U�B	X�B	V�B	V�B	W�B	^�B	`B	dB	c B	`�B	`�B	c B	eB	f2B	j0B	mCB	q[B	v�B	x�B	z�B	|�B	��B	��B	��B	��B	��B	�B	�@B	� B	�B	� B	�?B	�?B	�KB	�_B	�?B	�YB	�YB	�_B	�eB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�4B	�OB	�[B	�GB	�MB	�SB	�tB	�SB	�mB	�SB	�lB	�xB	˒B	�~B	ΊB	ϑB	ϑB	бB	ѝB	ңB	өB	��B	ԯB	��B	ּB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	� B	�&B	�B	�B	�B	�B	�B	�B	�>B	�*B	�0B	�QB	�6B	�6B	�=B	�CB	�IB	�OB	�UB	�UB	�[B	�aB	�aB	�aB	�|B	�hB	�B	�nB	�nB	�nB	�tB	�tB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B	��B
�B
�B
�B
�B
�B
 �B
�B
�B
�B

	B
B
(B
B
B
B
B
"B
"B
B
B
"B
B
"B
B
B
(B
4B
4B
4B
B
B
B
&B
@B
 B
 B
4B
B
B
B
4B
B
B
 B
@B
&B
,B
,B
FB
2B
2B
9B
9B
9B
SB
YB
KB
KB
KB
QB
WB
qB
WB
WB
WB
]B
]B
]B
dB
dB
jB
pB
 vB
 vB
!|B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
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
)�B
)�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
.�B
.�B
.�B
/�B
/�B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
5B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
6B
6�B
6�B
6�B
8B
8B
9$B
9$B
:*B
:B
:*B
;B
;B
;B
;0B
;B
;0B
;B
;0B
<B
<6B
=<B
="B
="B
>BB
>(B
>(B
>(B
>(B
?HB
?.B
?.B
?HB
?.B
?.B
?HB
?HB
?HB
@OB
AUB
@OB
@4B
@4B
AUB
A;B
A;B
A;B
A;B
AUB
B[B
BAB
BAB
CaB
CGB
DgB
DMB
DMB
DgB
DMB
DgB
EmB
EmB
EmB
EmB
EmB
FYB
FYB
FYB
FYB
FYB
FYB
FYB
G_B
G_B
G_B
GzB
HfB
HfB
H�B
H�B
HfB
HfB
IlB
IlB
I�B
IlB
IlB
J�B
JrB
J�B
JrB
JrB
JrB
KxB
KxB
L~B
L~B
L~B
L~B
L�B
L~B
L~B
L~B
M�B
M�B
M�B
M�B
M�B
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
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
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
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
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
[�B
[�B
[�B
[�B
[�B
[�B
\�B
^B
]�B
^B
]�B
^�B
_B
_B
_�B
_�B
_�B
_�B
`�B
`�B
aB
`�B
`�B
bB
bB
a�B
a�B
a�B
a�B
a�B
cB
cB
c B
c B
cB
dB
dB
d&B
d&B
d&B
dB
d&B
d&B
dB
dB
e,B
eB
e,B
f2B
f2B
fB
fB
f2B
fB
fB
f2B
fB
fB
f2B
gB
gB
gB
g8B
gB
gB
gB
g8B
gB
gB
gB
gB
gB
h$B
h$B
h>B
h$B
iDB
i*B
i*B
i*B
iDB
i*B
i*B
i*B
iDB
jKB
j0B
j0B
j0B
j0B
kQB
k6B
kQB
k6B
k6B
kQB
k6B
l=B
l=B
lWB
lWB
lWB
l=B
lWB
mCB
mCB
m]B
m]B
mCB
m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.54(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806220043432018062200434320180622004343201806230030352018062300303520180623003035JA  ARFMdecpA19c                                                                20180617123516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180617033518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180617033518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180617033519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180617033519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180617033519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180617033520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180617033520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180617033520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180617033520                      G�O�G�O�G�O�                JA  ARUP                                                                        20180617035459                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180617154345  CV  JULD            G�O�G�O�F�W�                JM  ARCAJMQC2.0                                                                 20180621154343  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180621154343  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180622153035  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                