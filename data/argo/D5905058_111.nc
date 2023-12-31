CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-17T21:36:19Z creation;2018-12-17T21:36:23Z conversion to V3.1;2019-12-23T06:10:09Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181217213619  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA  I2_0675_111                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؘ�E�1   @ؘ�b� @7��+j���c>S��Mj1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A�\A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
p�B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=Bz
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%�=D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD�ӅD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�S�D��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�PRDƐRD��RD�RD�PRDǐRD��RD�RD�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�RD�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD�RD�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�RD�PRDڐRD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�S�DD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD��D�PRD�RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�S�D���D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�JA���A�A�VA�
=A�{A��A��A�{A��A�/A�1'A�/A�-A�$�A�+A�+A�/A�33A�5?A�7LA�9XA�9XA�9XA�9XA�7LA�33A�9XA�5?A�(�A�$�A��A�  A���A�AƲ-Aƣ�AƋDAƁA�jA��#A�?}Að!A�ZA���A��FA��A�Q�A�A��A�7LA��uA�VA�oA���A�E�A�r�A��wA�C�A��mA�-A���A�ZA���A�ĜA��`A�M�A��#A��7A��9A�I�A��A��A��A�E�A���A�n�A��A�-A��9A�+A�~�A�jA��+A��A�C�A�VA��A�VA��DA���A���A��HA�S�A�jA�hsA�z�A��FA��wA�A�=qA�n�A��9A���A�K�A��yA��\A�S�A�p�A��+A�/A��`A�Au�^At{Aqx�ApbAmt�AlM�Al(�Ak�Ai�
Ag�hAe�Ad��Ab�/A`ĜA`�A]�
AZ��AXffAW33AV�DAT��AQ�wAQt�AQO�AP��AO��AM�-AK��AI��AG��AF1AEoAC�wAB�!ABJA@��A?�A=�A<v�A;"�A:�+A9�^A8�+A7��A7O�A6�A5��A5&�A4�9A3�A2z�A1��A0ȴA01'A/t�A.�RA-��A+��A*��A)�#A'�A&5?A%hsA$��A#�A"��A!�TA!hsA ĜA =qA&�AE�AK�AjA��AE�A�jA-A��A��A�/A�AJAG�A�+A1'AĜAVAn�A1'A��AG�A
�`A
�9A	��A	XA	+A�jAA�AG�A~�A��A%A�
AS�A��AA�A�A �A �+A 1'@�ff@�x�@��u@�\)@��T@�C�@�%@�S�@�@�`B@� �@���@��@�!@��@��@��@�Ĝ@�j@�1'@�ƨ@�R@�9@�M�@��@��@ؼj@���@և+@�hs@ԃ@җ�@мj@���@�-@���@�`B@�&�@̼j@˥�@ʗ�@�%@�  @��y@�=q@��T@őh@�G�@���@�
=@�`B@�bN@�1@�;d@��\@��@�p�@�%@��9@�Z@���@�ȴ@���@��+@�M�@���@���@�b@�o@�~�@�@�@�O�@�%@�(�@���@�5?@��@��7@��@�bN@��@��F@���@�t�@��@�E�@���@�7L@��/@��j@��u@�z�@�Z@�A�@�9X@��m@�|�@�o@��!@�J@���@�%@��@��@�A�@��m@��P@�S�@�33@���@���@�V@�J@���@�O�@�Ĝ@�bN@�1'@��@��F@�S�@�@��y@��+@�E�@�{@��-@�x�@�/@���@��@�Q�@�1@��@��;@��w@�l�@�
=@���@�v�@�E�@���@���@���@�&�@���@�Ĝ@���@��@�t�@�C�@��@�5?@���@��#@���@�@��-@�O�@��@� �@��;@�t�@�\)@�C�@�"�@���@�v�@�-@�@���@�@��^@���@��@��`@��j@���@�z�@�Z@�Q�@��m@�"�@�M�@�-@�{@���@���@��#@�5?@�5?@�5?@���@��@�hs@�O�@�&�@�&�@���@�Q�@�I�@�1'@��@��@���@�+@�l�@�C�@�
=@�33@�C�@�33@���@�v�@�E�@��-@�?}@�/@�%@���@�z�@�Q�@��@���@��F@��P@�l�@�S�@�"�@��@���@�n�@�V@��@��@���@���@��7@�G�@���@���@��`@��D@�z�@� �@�  @��
@�|�@�K�@�;d@��@���@��!@���@�V@�{@��@�@�G�@�V@���@��D@�bN@�(�@��F@�
=@��H@��@�ȴ@���@�~�@�=q@��@�{@�J@��@��#@��-@�/@���@��j@��@���@��u@��D@��@�r�@�I�@�9X@�1'@�  @\)@+@+@~��@~�R@~E�@}p�@|��@|�@|(�@{�
@{�F@{dZ@{S�@{S�@{"�@{o@{@{@z��@z�!@z~�@z�@y�^@y��@y��@y��@y�7@yx�@y&�@xQ�@w��@wl�@w;d@v��@v$�@u`B@uO�@u`B@u�h@u�@t�@t�@tZ@t(�@s��@r��@r�!@r�\@r^5@r=q@rJ@q��@p��@p  @o|�@o+@n��@n$�@m�@m/@l9X@k��@kS�@k"�@j�@j��@j�\@j-@i�#@i�^@i�7@iG�@h��@hĜ@h�9@h�@hA�@h  @g�w@g�P@g;d@f�@fE�@f{@f@e�T@e�-@e�h@e�h@e�@d��@d9X@c�
@ct�@co@b��@bn�@bJ@a�7@a7L@a%@`��@`Ĝ@`��@`�@`A�@`  @_|�@_K�@_
=@^�+@]�T@]�-@]�@\�/@\�@\Z@[t�@Z�H@Z�\@Z�@Y�#@Y�^@Yx�@YG�@Y7L@X��@X�u@X1'@W��@WK�@V�y@Vv�@V5?@V{@U�@T�/@T��@Tj@S��@S�
@SS�@R�!@R=q@Q��@Q��@Q�7@Q�@P�u@P  @O�w@O��@Ol�@O
=@N��@N�+@N�+@N�+@N�+@NV@NE�@M�@M`B@L��@L�D@LZ@L9X@K�m@K33@J��@JM�@I��@Ix�@I%@H�u@HbN@HbN@HA�@G�@G��@G�P@Gl�@G�@F�R@Fff@E�T@E�h@E`B@EO�@E?}@D��@D�/@D�/@D��@D�j@D�@Dj@C��@C�F@Ct�@CC�@C33@C33@Co@C@B��@B�\@B~�@A��@A�^@A��@Ax�@AX@A�@@�@@1'@@  @?��@?�@?�@?�P@?K�@>��@>��@>$�@=�-@=�@=O�@<�j@<z�@<I�@<9X@;��@;�F@;t�@;"�@:��@:n�@:�@9�#@9x�@9X@97L@8��@8��@8�u@8Q�@8b@7�@7�@7��@7l�@7�@6��@6ȴ@6v�@65?@6@5�-@5�@5�@5p�@5O�@5�@4��@4�/@4��@4�j@3�
@3t�@3"�@3@2�@2��@2�!@2n�@2=q@2-@1�#@1G�@1&�@1�@0�`@0Ĝ@0Q�@01'@01'@0  @/�;@/�w@/�w@/�@/l�@/;d@/�@/�@.�y@.��@.E�@.E�@.E�@.5?@-�@-O�@-V@,��@,�@,��@,�D@,z�@,I�@,9X@,1@+ƨ@+dZ@+o@*�!@*=q@*J@)�#@)�^@)7L@(��@(Ĝ@(Q�@(1'@(b@(  @'�@'�@&ȴ@&�+@&v�@&E�@&{@%��@%�@%�@$�@$��@$��@$9X@#��@"��@"~�@"M�@"�@"J@!��@!7L@ �`@ ��@ r�@ A�@  �@�;@�P@;d@+@��@�y@ȴ@�R@��@ff@E�@5?@5?@5?@$�@{@@�@�-@p�@`B@O�@?}@V@�@�D@Z@9X@1@�m@�m@ƨ@��@�@t�@33@�@��@�!@��@-@��@G�@�`@�u@bN@b@�@�@��@|�@|�@|�@;d@�+@$�@�@�T@��@@�h@�@`B@��@�@�D@z�@j@(�@��@ƨ@��@dZ@o@�H@�!@~�@^5@�@��@�7@X@G�@��@��@Ĝ@�9@�u@r�@bN@ �@�;@l�@�@��@�R@��@��@��@��@�+@v�@ff@5?@�-@��@��@��@`B@�@�/@�j@z�@�@�m@�
@ƨ@�F@dZ@"�@
�@
��@
��@
�!@
�!@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�JA���A�A�VA�
=A�{A��A��A�{A��A�/A�1'A�/A�-A�$�A�+A�+A�/A�33A�5?A�7LA�9XA�9XA�9XA�9XA�7LA�33A�9XA�5?A�(�A�$�A��A�  A���A�AƲ-Aƣ�AƋDAƁA�jA��#A�?}Að!A�ZA���A��FA��A�Q�A�A��A�7LA��uA�VA�oA���A�E�A�r�A��wA�C�A��mA�-A���A�ZA���A�ĜA��`A�M�A��#A��7A��9A�I�A��A��A��A�E�A���A�n�A��A�-A��9A�+A�~�A�jA��+A��A�C�A�VA��A�VA��DA���A���A��HA�S�A�jA�hsA�z�A��FA��wA�A�=qA�n�A��9A���A�K�A��yA��\A�S�A�p�A��+A�/A��`A�Au�^At{Aqx�ApbAmt�AlM�Al(�Ak�Ai�
Ag�hAe�Ad��Ab�/A`ĜA`�A]�
AZ��AXffAW33AV�DAT��AQ�wAQt�AQO�AP��AO��AM�-AK��AI��AG��AF1AEoAC�wAB�!ABJA@��A?�A=�A<v�A;"�A:�+A9�^A8�+A7��A7O�A6�A5��A5&�A4�9A3�A2z�A1��A0ȴA01'A/t�A.�RA-��A+��A*��A)�#A'�A&5?A%hsA$��A#�A"��A!�TA!hsA ĜA =qA&�AE�AK�AjA��AE�A�jA-A��A��A�/A�AJAG�A�+A1'AĜAVAn�A1'A��AG�A
�`A
�9A	��A	XA	+A�jAA�AG�A~�A��A%A�
AS�A��AA�A�A �A �+A 1'@�ff@�x�@��u@�\)@��T@�C�@�%@�S�@�@�`B@� �@���@��@�!@��@��@��@�Ĝ@�j@�1'@�ƨ@�R@�9@�M�@��@��@ؼj@���@և+@�hs@ԃ@җ�@мj@���@�-@���@�`B@�&�@̼j@˥�@ʗ�@�%@�  @��y@�=q@��T@őh@�G�@���@�
=@�`B@�bN@�1@�;d@��\@��@�p�@�%@��9@�Z@���@�ȴ@���@��+@�M�@���@���@�b@�o@�~�@�@�@�O�@�%@�(�@���@�5?@��@��7@��@�bN@��@��F@���@�t�@��@�E�@���@�7L@��/@��j@��u@�z�@�Z@�A�@�9X@��m@�|�@�o@��!@�J@���@�%@��@��@�A�@��m@��P@�S�@�33@���@���@�V@�J@���@�O�@�Ĝ@�bN@�1'@��@��F@�S�@�@��y@��+@�E�@�{@��-@�x�@�/@���@��@�Q�@�1@��@��;@��w@�l�@�
=@���@�v�@�E�@���@���@���@�&�@���@�Ĝ@���@��@�t�@�C�@��@�5?@���@��#@���@�@��-@�O�@��@� �@��;@�t�@�\)@�C�@�"�@���@�v�@�-@�@���@�@��^@���@��@��`@��j@���@�z�@�Z@�Q�@��m@�"�@�M�@�-@�{@���@���@��#@�5?@�5?@�5?@���@��@�hs@�O�@�&�@�&�@���@�Q�@�I�@�1'@��@��@���@�+@�l�@�C�@�
=@�33@�C�@�33@���@�v�@�E�@��-@�?}@�/@�%@���@�z�@�Q�@��@���@��F@��P@�l�@�S�@�"�@��@���@�n�@�V@��@��@���@���@��7@�G�@���@���@��`@��D@�z�@� �@�  @��
@�|�@�K�@�;d@��@���@��!@���@�V@�{@��@�@�G�@�V@���@��D@�bN@�(�@��F@�
=@��H@��@�ȴ@���@�~�@�=q@��@�{@�J@��@��#@��-@�/@���@��j@��@���@��u@��D@��@�r�@�I�@�9X@�1'@�  @\)@+@+@~��@~�R@~E�@}p�@|��@|�@|(�@{�
@{�F@{dZ@{S�@{S�@{"�@{o@{@{@z��@z�!@z~�@z�@y�^@y��@y��@y��@y�7@yx�@y&�@xQ�@w��@wl�@w;d@v��@v$�@u`B@uO�@u`B@u�h@u�@t�@t�@tZ@t(�@s��@r��@r�!@r�\@r^5@r=q@rJ@q��@p��@p  @o|�@o+@n��@n$�@m�@m/@l9X@k��@kS�@k"�@j�@j��@j�\@j-@i�#@i�^@i�7@iG�@h��@hĜ@h�9@h�@hA�@h  @g�w@g�P@g;d@f�@fE�@f{@f@e�T@e�-@e�h@e�h@e�@d��@d9X@c�
@ct�@co@b��@bn�@bJ@a�7@a7L@a%@`��@`Ĝ@`��@`�@`A�@`  @_|�@_K�@_
=@^�+@]�T@]�-@]�@\�/@\�@\Z@[t�@Z�H@Z�\@Z�@Y�#@Y�^@Yx�@YG�@Y7L@X��@X�u@X1'@W��@WK�@V�y@Vv�@V5?@V{@U�@T�/@T��@Tj@S��@S�
@SS�@R�!@R=q@Q��@Q��@Q�7@Q�@P�u@P  @O�w@O��@Ol�@O
=@N��@N�+@N�+@N�+@N�+@NV@NE�@M�@M`B@L��@L�D@LZ@L9X@K�m@K33@J��@JM�@I��@Ix�@I%@H�u@HbN@HbN@HA�@G�@G��@G�P@Gl�@G�@F�R@Fff@E�T@E�h@E`B@EO�@E?}@D��@D�/@D�/@D��@D�j@D�@Dj@C��@C�F@Ct�@CC�@C33@C33@Co@C@B��@B�\@B~�@A��@A�^@A��@Ax�@AX@A�@@�@@1'@@  @?��@?�@?�@?�P@?K�@>��@>��@>$�@=�-@=�@=O�@<�j@<z�@<I�@<9X@;��@;�F@;t�@;"�@:��@:n�@:�@9�#@9x�@9X@97L@8��@8��@8�u@8Q�@8b@7�@7�@7��@7l�@7�@6��@6ȴ@6v�@65?@6@5�-@5�@5�@5p�@5O�@5�@4��@4�/@4��@4�j@3�
@3t�@3"�@3@2�@2��@2�!@2n�@2=q@2-@1�#@1G�@1&�@1�@0�`@0Ĝ@0Q�@01'@01'@0  @/�;@/�w@/�w@/�@/l�@/;d@/�@/�@.�y@.��@.E�@.E�@.E�@.5?@-�@-O�@-V@,��@,�@,��@,�D@,z�@,I�@,9X@,1@+ƨ@+dZ@+o@*�!@*=q@*J@)�#@)�^@)7L@(��@(Ĝ@(Q�@(1'@(b@(  @'�@'�@&ȴ@&�+@&v�@&E�@&{@%��@%�@%�@$�@$��@$��@$9X@#��@"��@"~�@"M�@"�@"J@!��@!7L@ �`@ ��@ r�@ A�@  �@�;@�P@;d@+@��@�y@ȴ@�R@��@ff@E�@5?@5?@5?@$�@{@@�@�-@p�@`B@O�@?}@V@�@�D@Z@9X@1@�m@�m@ƨ@��@�@t�@33@�@��@�!@��@-@��@G�@�`@�u@bN@b@�@�@��@|�@|�@|�@;d@�+@$�@�@�T@��@@�h@�@`B@��@�@�D@z�@j@(�@��@ƨ@��@dZ@o@�H@�!@~�@^5@�@��@�7@X@G�@��@��@Ĝ@�9@�u@r�@bN@ �@�;@l�@�@��@�R@��@��@��@��@�+@v�@ff@5?@�-@��@��@��@`B@�@�/@�j@z�@�@�m@�
@ƨ@�F@dZ@"�@
�@
��@
��@
�!@
�!@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�VB�\B�VB�VB�VB�VB�VB�VB�VB�VB�\B�\B�\B�\B�VB�VB�PB�PB�VB�\B�\B�bB�\B�oB�oB�uB��B�uB�{B��B��B��B��B�3B�LB�wBȴB��B�B�TB1B;dBH�BJ�BM�BVBp�B�+B�PB��B��B��B��B��B��B��B��B��B��B��B�hB�\B�VB�=B�7B�bB�PB�DB�7B�%B�B� B{�Bp�BjB_;B]/BW
BP�BG�BB�B:^B/B!�B�BPB��B�HB�B��B��B��B�-B��B��B�Bt�Be`B:^B�BuB1B
��B
�fB
��B
B
�LB
��B
��B
�hB
�=B
�B
S�B	��B	�B	�B	�}B	��B	�dB	�}B	�RB	�B	��B	��B	��B	��B	�%B	� B	u�B	`BB	N�B	D�B	=qB	9XB	$�B	!�B	�B	�B	�B	\B	%B��B��B�B�B�yB�TB�BB�/B�
B��B��BǮBĜB��B�qB�XB�RB�-B�'B�B�B�B��B��B��B��B��B�{B�bB�=B�%B�B{�Bw�Bs�Bq�Bp�Bm�BiyBhsBffBdZBaHB_;B\)BYBW
BR�BP�BN�BL�BK�BI�BC�BB�B?}B=qB;dB<jB7LB6FB5?B49B49B2-B2-B1'B/B/B/B-B-B+B)�B(�B'�B&�B%�B%�B$�B$�B"�B"�B"�B �B!�B �B�B �B�B�B�B�B�B�B�B�B!�B!�B!�B!�B"�B"�B#�B#�B%�B(�B'�B(�B&�B%�B&�B%�B%�B(�B+B/B0!B1'B33B33B49B8RB:^B>wBA�BD�BE�BE�BE�BE�BF�BL�BO�BQ�BQ�BT�BVBXBXBYBZB[#B]/B_;B_;B`BB`BBbNBcTBgmBk�Bm�Bo�Bo�Bq�Bq�Bu�Bx�B{�B|�B~�B�B�B�B�B�B�%B�7B�DB�PB�bB�hB�oB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�9B�?B�^B�qB��BȴB��B��B��B��B�
B�B�5B�BB�`B�B�B�B��B��B��B��B	B	1B	
=B	PB	VB	bB	{B	�B	�B	�B	!�B	$�B	%�B	(�B	)�B	-B	/B	2-B	49B	49B	5?B	7LB	9XB	;dB	<jB	=qB	?}B	F�B	I�B	H�B	H�B	I�B	K�B	K�B	M�B	P�B	R�B	S�B	VB	YB	[#B	[#B	]/B	aHB	cTB	dZB	dZB	gmB	hsB	hsB	l�B	o�B	n�B	n�B	o�B	q�B	u�B	y�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�'B	�-B	�?B	�FB	�LB	�RB	�^B	�dB	�dB	�dB	�jB	�jB	�qB	�wB	��B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�NB	�NB	�TB	�`B	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B
DB
DB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
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
.B
/B
0!B
0!B
0!B
0!B
0!B
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
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
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
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
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
C�B
C�B
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
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
H�B
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
J�B
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
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
[#B
\)B
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
]/B
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
bNB
cTB
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�"B�(B�<B�"B�<B�"B�<B�<B�"B�"B�(B�(B�(B�(B�<B�<B�B�6B�<B�(B�(B�.B�BB�:B�:B�[B�MB�@B�FB�_B�eB��B��B��B�2B�BBȀBҽB��B� B�B;JBH�BJ�BM�BU�BpoB��B�6B�B�B�eB�qB�_B�YB�SB�mB�SB�MB�MB�4B�(B�"B�	B�B�.B�B�B�B��B��B�B{�BpoBjKB_B\�BV�BP�BG�BBuB:*B.�B!�BeB6B�B�B��B˒B�OB�OB��B��B��B��Bt�Be,B:*B~B@B�B
��B
�2B
ʌB
�[B
�B
��B
�qB
�4B
�	B
��B
S�B	��B	�]B	��B	�HB	�UB	�0B	�HB	�B	��B	��B	�xB	�_B	�MB	��B	�B	u�B	`B	N�B	DgB	=<B	9$B	$�B	!�B	�B	~B	YB	(B	�B��B��B�oB�cB�DB� B�B��B��BҽB͟B�zB�gB�UB�<B�$B�B��B��B��B��B��B��B��B��B�qB�YB�FB�.B��B��B��B{�Bw�Bs�BqvBpoBm]BiDBh>Bf2BdBaB^�B[�BX�BV�BR�BP�BN�BL~BKxBI�BCaBBAB?HB="B;B<6B6�B6B5B4B4B1�B1�B0�B.�B.�B.�B,�B,�B*�B)�B(�B'�B&�B%�B%�B$�B$�B"�B"�B"�B �B!�B �B�B �BpBjB~B�B�B~B�B�B!�B!�B!�B!|B"�B"�B#�B#�B%�B(�B'�B(�B&�B%�B&�B%�B%�B(�B*�B.�B/�B0�B2�B2�B4B8B:B>BBAUBDMBESBEmBEmBESBFtBL�BO�BQ�BQ�BT�BU�BW�BW�BX�BY�BZ�B\�B^�B^�B_�B_�BbBc Bg8Bk6BmCBoOBoiBq[BqvButBx�B{�B|�B~�B��B��B��B��B��B��B�B��B�B�B�4B�:B�:B�@B�FB�FB�2B�?B�KB�kB�xB��B�|B��B��B��B��B��B��B��B��B�B�B�*B�<B�4BȀB̈́BϫBңBөBּB��B��B�B�B�6B�cB�|B�tB��B��B��B	�B	�B		�B	B	"B	B	FB	EB	kB	]B	!|B	$�B	%�B	(�B	)�B	,�B	.�B	1�B	3�B	3�B	4�B	7B	9$B	;B	<6B	="B	?.B	FtB	I�B	HfB	H�B	I�B	K�B	K�B	M�B	P�B	R�B	S�B	U�B	X�B	Z�B	Z�B	\�B	`�B	c B	d&B	d&B	g8B	h>B	h$B	l=B	oiB	nIB	nIB	oiB	qvB	u�B	y�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�(B	�B	�@B	�,B	�SB	�SB	�YB	�KB	�]B	�~B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�6B	�"B	�BB	�4B	�[B	�MB	�gB	�SB	�SB	�YB	ɆB	�rB	˒B	�~B	͟B	ΊB	ΊB	бB	ѝB	ңB	ңB	ԯB	��B	��B	��B	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	� B	��B	�B	�B	�B	�DB	�DB	�DB	�DB	�0B	�0B	�KB	�0B	�0B	�KB	�=B	�=B	�WB	�]B	�IB	�IB	�cB	�cB	�OB	�oB	�vB	�hB	�B	�B	�nB	�nB	��B	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

	B
B

�B
B
�B
B
B
"B
B
B
(B
(B
(B
(B
.B
B
B
B
B
 B
 B
:B
:B
&B
@B
@B
,B
FB
,B
MB
MB
MB
2B
2B
MB
2B
MB
9B
SB
YB
_B
EB
KB
eB
KB
QB
kB
kB
kB
QB
QB
WB
qB
qB
]B
]B
]B
~B
dB
dB
�B
�B
�B
jB
�B
 vB
 �B
 vB
!|B
!�B
!|B
!|B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
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
-�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
4B
3�B
3�B
3�B
4�B
5B
4�B
6B
6B
5�B
6B
5�B
6B
5�B
6B
5�B
6B
6�B
6�B
7B
8B
8B
8B
8B
8B
8B
8B
8B
9	B
9$B
:B
:*B
:*B
:B
:*B
;B
;0B
<B
<B
<6B
<B
<6B
<B
<6B
<B
="B
="B
>BB
>BB
?.B
?HB
?HB
?.B
?.B
?.B
@4B
@4B
@4B
AUB
A;B
AUB
B[B
BAB
B[B
CaB
CGB
CaB
CaB
DMB
DMB
DMB
DgB
DgB
DMB
DMB
ESB
EmB
ESB
ESB
ESB
FtB
FYB
FtB
FYB
FYB
FYB
FtB
FYB
FtB
H�B
H�B
HfB
H�B
HfB
HfB
H�B
HfB
IlB
I�B
IlB
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
JrB
JrB
JrB
J�B
KxB
KxB
K�B
KxB
K�B
K�B
KxB
L�B
L~B
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
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
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
Z�B
[�B
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
\�B
\�B
^B
]�B
]�B
_B
_B
^�B
_B
_B
_�B
`B
aB
`�B
`�B
a�B
bB
bB
a�B
a�B
a�B
a�B
a�B
c B
d&B
d&B
dB
dB
d&B
d&B
dB
d&B
e,B
eB
e,B
eB
e,B
fB
fB
f2B
f2B
f2B
g8B
gB
gB
gB
h$B
h$B
h$B
h$B
i*B
iDB
i*B
iDB
i*B
i*B
i*B
iDB
j0B
j0B
jKB
jKB
k6B
k6B
k6B
kQB
kQB
k6B
k6B
k6B
kQB
k6B
l=B
lWB
lWB
lWB
lWB
l=B
l=B
m]B
mCB
m]B
ncB
nIB
ncB
nIB
nIB
ncB
nIB
oOB
oiB
oOB
oiB
oiB
oi11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.51(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812230039452018122300394520181223003945201812240032362018122400323620181224003236JA  ARFMdecpA19c                                                                20181218063617  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181217213619  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181217213621  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181217213621  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181217213622  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181217213622  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181217213622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181217213622  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181217213622  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181217213623                      G�O�G�O�G�O�                JA  ARUP                                                                        20181217215546                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181218153653  CV  JULD            G�O�G�O�F��v                JM  ARCAJMQC2.0                                                                 20181222153945  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181222153945  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181223153236  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                