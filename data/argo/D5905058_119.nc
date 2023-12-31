CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-21T00:37:17Z creation;2019-01-21T00:37:20Z conversion to V3.1;2019-12-23T06:08:18Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20190121003717  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  I2_0675_119                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ءo�(3�1   @ءp�[�@7��u%�c1��o 1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�3D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
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
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�D�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�PRDƐRD��RD�RD�PRDǐRD��RD�RD�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�RD�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD��D�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�RD�PRDڐRD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD꓅D��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�+A�+A�(�A�/A�33A�/A�33A�1'A�1'A�9XA�/A�(�A�+A�9XA�v�A��+A��7A���A�ȴA���A��/A��
A���A��A�p�A�bNA�\)A�ZA�ZA�\)A�^5A�bNA�p�A�x�A��7A��!A���A���A��mA�1A�-A�1'A�E�A�+A���A�=qA���A��`A��wA��\A�1'A��mA�ƨA�|�A��A�JA��/A�K�A�5?A��#A��RA���A���A�dZA�=qA��FA���A��+A�K�A���A���A�A�S�A�A�z�A��A�z�A���A�XA�&�A�XA��TA��A�A���A��PA��uA��A��#A���A��9A�v�A�dZA�t�A�"�A��mA�Q�A��^A��
A���A�33A���A�JA�;dA�JA�ZA�VA���A���A��9A���A�O�A���A��A���A�A��/A�bA��!A�x�A}�hA|ȴA{�Az��Au��As�#Ao��Am�Alv�AkVAg�TAeAc�A`1A\ĜA[�7AZ�9AYG�AU�;ATz�AS��AQO�AO;dAN�AMS�AJ-AH=qAFE�AEx�AE
=AC��AB{A?�A>  A=33A<ffA<A�A<bNA;|�A:~�A8��A8�A7S�A6n�A5��A5\)A3��A2��A1|�A0 �A/C�A-�mA,�HA+��A+A)�
A(�A'��A&r�A%��A$�DA#hsA"�`A"JA ��A�A�mA�yA�A33A��A-A�
A�AC�A�Av�A`BA��Ax�A33A�uA^5A��Al�AĜA9XA��Ax�A��A9XA��AȴA��AA
n�A	�-AM�A��A�PAC�An�AO�A�!A1'A�hAA��An�A ��@���@��/@�|�@�@���@�=q@�/@���@�$�@�&�@�1'@�@�C�@�hs@�~�@�@�j@�F@���@蛦@�1@�K�@��@�n�@��@�X@�b@�^5@�$�@�h@� �@�"�@��@�7L@���@܋D@��m@ڧ�@��#@�7L@��m@�E�@ա�@�bN@���@�(�@θR@��@́@�z�@��H@�X@�(�@�"�@š�@�A�@§�@��@���@��+@��-@�p�@�A�@�K�@��\@�`B@��u@��@���@�S�@�"�@��@��#@���@�I�@��
@�+@��@���@��@�X@�%@��j@�ƨ@�l�@�dZ@�C�@��y@�n�@�@�?}@��/@���@��j@�I�@��w@�S�@�+@��H@��\@�$�@��@�x�@��@��@��m@�
=@���@�-@��#@�7L@���@��`@���@�Z@��@���@�\)@�;d@�
=@���@���@�~�@�V@��@�`B@�Ĝ@��D@�A�@��@��F@�|�@���@��H@�M�@��@�hs@��@�I�@��
@��@�\)@�+@�
=@��y@�ff@���@��7@�?}@��@�Q�@�9X@�9X@��@���@���@�|�@�
=@��H@���@��+@�M�@�E�@��@��#@�/@�7L@�p�@�x�@��@��@��-@���@�p�@��^@��-@�hs@��`@��`@���@�  @���@���@�1@��u@�r�@�A�@�b@�b@��
@�ƨ@��w@���@�33@��@�{@��@��T@��-@���@���@�x�@�?}@���@�%@�X@�G�@���@��D@�A�@��@���@�t�@�dZ@�dZ@�S�@�+@�"�@���@�ȴ@�^5@�E�@���@��#@�p�@�7L@��@�V@��`@��D@�Z@�(�@��m@��@�+@�
=@��@���@�^5@�M�@�=q@�5?@��@��^@�p�@�X@�&�@�%@��j@�A�@���@�K�@�"�@�
=@��!@�V@�M�@�$�@��@��@��^@���@�hs@�&�@��@���@��`@���@��D@�r�@�Q�@���@��
@��P@�t�@�l�@�\)@�;d@��@���@��+@�~�@�~�@�ff@�M�@�$�@���@��^@��7@�7L@�%@���@��j@���@�r�@�I�@�b@�;@�w@�P@
=@~��@~V@~@}�T@}�T@}��@}��@}�@}/@|�j@|Z@|9X@|I�@|9X@{�F@z��@z�\@z-@z�@z�@z�@zJ@zJ@zJ@y�#@y�^@y��@y&�@x�@xA�@w�@wl�@w
=@v�R@vff@vE�@u��@u/@t��@tz�@tI�@s��@sƨ@s33@r��@r�!@r�\@r=q@rJ@q��@q7L@p�9@pr�@o�w@oK�@o
=@n�y@n��@nff@n5?@m��@m�@m/@l�j@lz�@l9X@k�
@k��@kt�@k33@j��@i�7@i&�@i%@h�`@hA�@hb@g��@g�@g|�@g|�@g;d@f��@f�@f��@fff@f@e��@e�@eO�@e/@d��@d��@dj@d�@c��@c"�@c@b�H@b�!@bM�@a�^@ahs@a�@`��@`A�@_�@_|�@_
=@^�y@^ȴ@^v�@]�h@\��@\I�@\1@[��@[S�@[C�@Z�@Z��@Y�@Y��@YG�@Y�@XĜ@X�9@X�@XA�@W�@W�@WK�@Vȴ@V��@Vv�@V5?@V@U@UO�@T�D@T�@T1@Sƨ@SdZ@R��@Rn�@R-@Q��@Q��@QX@Q�@P��@PA�@O�;@O�P@O\)@O+@O�@O
=@N�R@N5?@M��@M�@M�@L�j@L��@L�D@Lj@L(�@K�
@K��@K��@Kt�@KdZ@KC�@K@J�H@J-@I��@I�#@I��@I�7@I&�@H��@HQ�@G��@Gl�@G
=@Fȴ@F�R@F��@FV@F5?@E�@E��@E�-@Ep�@D�@D��@DI�@D�@C��@Ct�@C33@B�H@B~�@B�@A�@A�#@A�7@A7L@A�@@��@@Ĝ@@�9@@r�@@  @?��@?K�@?�@?
=@>�y@>V@=�T@=�h@=O�@=?}@=V@<�@<��@<�@<j@<�@;��@;ƨ@;��@;t�@;S�@;@:�@:�!@:^5@9�@9�^@9��@9x�@9�@8�9@8��@8�@8Q�@8  @7�;@7�P@7\)@7;d@6ȴ@6�+@65?@6$�@5�@5��@5�h@5p�@5/@4�j@4z�@4Z@4�@3ƨ@3C�@2�H@2~�@2^5@2�@1��@1�7@1X@0��@0��@0�u@0�@0r�@0Q�@0Q�@0b@/�P@/K�@/+@/
=@.ȴ@.��@.��@.�+@.E�@-�@-@-��@-��@-��@-�@-`B@-O�@-/@,�@,�@,��@,��@,j@+��@+�
@+�F@+��@+�@+dZ@+33@+o@*�H@*��@*~�@*-@)�@)x�@)%@(�9@(�@(1'@'�@'|�@'\)@'K�@'�@&�@&�+@&$�@%�T@%�h@%`B@%O�@%V@$�@$��@$j@$1@#��@#C�@"�@"�!@"��@"�\@"^5@"M�@"M�@"-@!�#@!��@!hs@ �`@ ��@ r�@ bN@ Q�@  �@   @�w@��@l�@+@�y@��@5?@@�h@�h@p�@�@�/@�D@9X@�m@��@S�@@��@=q@-@�@��@G�@Ĝ@bN@ �@�;@��@��@�@l�@+@�y@�R@�+@5?@��@�h@?}@��@�@�/@��@�j@�@z�@(�@1@�m@ƨ@ƨ@��@��@S�@@�H@��@�!@��@M�@J@�#@�^@��@x�@G�@&�@%@��@�`@�@  @��@�w@�P@;d@+@�@
=@�y@��@ff@$�@@@�@�@��@�@`B@`B@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�+A�+A�(�A�/A�33A�/A�33A�1'A�1'A�9XA�/A�(�A�+A�9XA�v�A��+A��7A���A�ȴA���A��/A��
A���A��A�p�A�bNA�\)A�ZA�ZA�\)A�^5A�bNA�p�A�x�A��7A��!A���A���A��mA�1A�-A�1'A�E�A�+A���A�=qA���A��`A��wA��\A�1'A��mA�ƨA�|�A��A�JA��/A�K�A�5?A��#A��RA���A���A�dZA�=qA��FA���A��+A�K�A���A���A�A�S�A�A�z�A��A�z�A���A�XA�&�A�XA��TA��A�A���A��PA��uA��A��#A���A��9A�v�A�dZA�t�A�"�A��mA�Q�A��^A��
A���A�33A���A�JA�;dA�JA�ZA�VA���A���A��9A���A�O�A���A��A���A�A��/A�bA��!A�x�A}�hA|ȴA{�Az��Au��As�#Ao��Am�Alv�AkVAg�TAeAc�A`1A\ĜA[�7AZ�9AYG�AU�;ATz�AS��AQO�AO;dAN�AMS�AJ-AH=qAFE�AEx�AE
=AC��AB{A?�A>  A=33A<ffA<A�A<bNA;|�A:~�A8��A8�A7S�A6n�A5��A5\)A3��A2��A1|�A0 �A/C�A-�mA,�HA+��A+A)�
A(�A'��A&r�A%��A$�DA#hsA"�`A"JA ��A�A�mA�yA�A33A��A-A�
A�AC�A�Av�A`BA��Ax�A33A�uA^5A��Al�AĜA9XA��Ax�A��A9XA��AȴA��AA
n�A	�-AM�A��A�PAC�An�AO�A�!A1'A�hAA��An�A ��@���@��/@�|�@�@���@�=q@�/@���@�$�@�&�@�1'@�@�C�@�hs@�~�@�@�j@�F@���@蛦@�1@�K�@��@�n�@��@�X@�b@�^5@�$�@�h@� �@�"�@��@�7L@���@܋D@��m@ڧ�@��#@�7L@��m@�E�@ա�@�bN@���@�(�@θR@��@́@�z�@��H@�X@�(�@�"�@š�@�A�@§�@��@���@��+@��-@�p�@�A�@�K�@��\@�`B@��u@��@���@�S�@�"�@��@��#@���@�I�@��
@�+@��@���@��@�X@�%@��j@�ƨ@�l�@�dZ@�C�@��y@�n�@�@�?}@��/@���@��j@�I�@��w@�S�@�+@��H@��\@�$�@��@�x�@��@��@��m@�
=@���@�-@��#@�7L@���@��`@���@�Z@��@���@�\)@�;d@�
=@���@���@�~�@�V@��@�`B@�Ĝ@��D@�A�@��@��F@�|�@���@��H@�M�@��@�hs@��@�I�@��
@��@�\)@�+@�
=@��y@�ff@���@��7@�?}@��@�Q�@�9X@�9X@��@���@���@�|�@�
=@��H@���@��+@�M�@�E�@��@��#@�/@�7L@�p�@�x�@��@��@��-@���@�p�@��^@��-@�hs@��`@��`@���@�  @���@���@�1@��u@�r�@�A�@�b@�b@��
@�ƨ@��w@���@�33@��@�{@��@��T@��-@���@���@�x�@�?}@���@�%@�X@�G�@���@��D@�A�@��@���@�t�@�dZ@�dZ@�S�@�+@�"�@���@�ȴ@�^5@�E�@���@��#@�p�@�7L@��@�V@��`@��D@�Z@�(�@��m@��@�+@�
=@��@���@�^5@�M�@�=q@�5?@��@��^@�p�@�X@�&�@�%@��j@�A�@���@�K�@�"�@�
=@��!@�V@�M�@�$�@��@��@��^@���@�hs@�&�@��@���@��`@���@��D@�r�@�Q�@���@��
@��P@�t�@�l�@�\)@�;d@��@���@��+@�~�@�~�@�ff@�M�@�$�@���@��^@��7@�7L@�%@���@��j@���@�r�@�I�@�b@�;@�w@�P@
=@~��@~V@~@}�T@}�T@}��@}��@}�@}/@|�j@|Z@|9X@|I�@|9X@{�F@z��@z�\@z-@z�@z�@z�@zJ@zJ@zJ@y�#@y�^@y��@y&�@x�@xA�@w�@wl�@w
=@v�R@vff@vE�@u��@u/@t��@tz�@tI�@s��@sƨ@s33@r��@r�!@r�\@r=q@rJ@q��@q7L@p�9@pr�@o�w@oK�@o
=@n�y@n��@nff@n5?@m��@m�@m/@l�j@lz�@l9X@k�
@k��@kt�@k33@j��@i�7@i&�@i%@h�`@hA�@hb@g��@g�@g|�@g|�@g;d@f��@f�@f��@fff@f@e��@e�@eO�@e/@d��@d��@dj@d�@c��@c"�@c@b�H@b�!@bM�@a�^@ahs@a�@`��@`A�@_�@_|�@_
=@^�y@^ȴ@^v�@]�h@\��@\I�@\1@[��@[S�@[C�@Z�@Z��@Y�@Y��@YG�@Y�@XĜ@X�9@X�@XA�@W�@W�@WK�@Vȴ@V��@Vv�@V5?@V@U@UO�@T�D@T�@T1@Sƨ@SdZ@R��@Rn�@R-@Q��@Q��@QX@Q�@P��@PA�@O�;@O�P@O\)@O+@O�@O
=@N�R@N5?@M��@M�@M�@L�j@L��@L�D@Lj@L(�@K�
@K��@K��@Kt�@KdZ@KC�@K@J�H@J-@I��@I�#@I��@I�7@I&�@H��@HQ�@G��@Gl�@G
=@Fȴ@F�R@F��@FV@F5?@E�@E��@E�-@Ep�@D�@D��@DI�@D�@C��@Ct�@C33@B�H@B~�@B�@A�@A�#@A�7@A7L@A�@@��@@Ĝ@@�9@@r�@@  @?��@?K�@?�@?
=@>�y@>V@=�T@=�h@=O�@=?}@=V@<�@<��@<�@<j@<�@;��@;ƨ@;��@;t�@;S�@;@:�@:�!@:^5@9�@9�^@9��@9x�@9�@8�9@8��@8�@8Q�@8  @7�;@7�P@7\)@7;d@6ȴ@6�+@65?@6$�@5�@5��@5�h@5p�@5/@4�j@4z�@4Z@4�@3ƨ@3C�@2�H@2~�@2^5@2�@1��@1�7@1X@0��@0��@0�u@0�@0r�@0Q�@0Q�@0b@/�P@/K�@/+@/
=@.ȴ@.��@.��@.�+@.E�@-�@-@-��@-��@-��@-�@-`B@-O�@-/@,�@,�@,��@,��@,j@+��@+�
@+�F@+��@+�@+dZ@+33@+o@*�H@*��@*~�@*-@)�@)x�@)%@(�9@(�@(1'@'�@'|�@'\)@'K�@'�@&�@&�+@&$�@%�T@%�h@%`B@%O�@%V@$�@$��@$j@$1@#��@#C�@"�@"�!@"��@"�\@"^5@"M�@"M�@"-@!�#@!��@!hs@ �`@ ��@ r�@ bN@ Q�@  �@   @�w@��@l�@+@�y@��@5?@@�h@�h@p�@�@�/@�D@9X@�m@��@S�@@��@=q@-@�@��@G�@Ĝ@bN@ �@�;@��@��@�@l�@+@�y@�R@�+@5?@��@�h@?}@��@�@�/@��@�j@�@z�@(�@1@�m@ƨ@ƨ@��@��@S�@@�H@��@�!@��@M�@J@�#@�^@��@x�@G�@&�@%@��@�`@�@  @��@�w@�P@;d@+@�@
=@�y@��@ff@$�@@@�@�@��@�@`B@`B@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�LB�LB�FB�FB�LB�XB�RB�XB�RB�RB�^B�RB�FB�LB�XBƨB��B��B�;B�TB�mB�B�B�B�B��B��B��B��B��B��B��B��BBBDB�B$�B&�B33BI�Be`Bo�B�B�+Bl�B\)BW
BXBYB\)BcTBhsBjBq�BbNBB�`B��B�mB+B�B�B�B�B��B��B��B  B�B�BDB{B>wBB�BE�BC�B5?B%�B%�B0!B"�B!�B�B�B�BuBB��B��B��B��B�B��B�}B�!B��B��B�'B��B�VB~�Bu�Bk�BQ�B=qB/B�BPBB
�B
�ZB
��B
ƨB
�wB
��B
�+B
s�B
gmB
]/B
M�B
2-B
,B
%�B
�B	��B	�TB	ɺB	�3B	��B	��B	�B	q�B	]/B	I�B	-B	&�B	�B	�B	B��B��B�`B�5B�B��B�}B�'B��B��B��B��B��B�hB��B��B�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�7B�B�B}�B{�Bw�Bt�Br�Bo�Bm�BiyBffBdZBbNB^5BZBXBVBT�BR�BQ�BQ�BP�BO�BN�BM�BL�BI�BF�BE�BE�BD�BC�BB�BA�B?}B>wB>wB<jB;dB:^B8RB6FB5?B2-B0!B.B-B,B,B+B)�B(�B'�B'�B&�B%�B$�B&�B$�B%�B$�B$�B#�B#�B#�B#�B#�B#�B$�B#�B"�B$�B"�B"�B#�B#�B&�B%�B%�B%�B%�B%�B%�B%�B(�B(�B(�B)�B+B+B-B-B-B-B.B/B0!B/B1'B2-B2-B33B6FB9XB;dB;dB;dB=qB?}BC�BD�BG�BK�BM�BO�BR�BVBXBZBZB^5B`BBaHBdZBffBhsBhsBiyBiyBjBn�Bq�Bs�Bt�Bw�Bx�By�B{�B}�B~�B� B�B�%B�%B�+B�1B�=B�JB�\B�hB�hB�hB�{B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�^B�wBŢBǮBɺBɺB��B��B�B�#B�/B�BB�ZB�fB�yB�B�B�B��B��B��B��B	  B	B	1B		7B	VB	bB	uB	�B	�B	�B	�B	�B	 �B	 �B	!�B	#�B	&�B	)�B	,B	0!B	2-B	33B	49B	8RB	<jB	=qB	?}B	@�B	A�B	C�B	E�B	G�B	J�B	L�B	O�B	R�B	VB	YB	[#B	\)B	]/B	ffB	jB	k�B	o�B	r�B	s�B	t�B	v�B	w�B	v�B	x�B	{�B	}�B	�B	�%B	�7B	�DB	�VB	�oB	�uB	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�FB	�XB	�^B	�dB	�qB	�}B	��B	��B	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
oB
oB
oB
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
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
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
/B
/B
/B
/B
/B
/B
0!B
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
2-B
2-B
2-B
2-B
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
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
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
C�B
C�B
C�B
D�B
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
H�B
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
L�B
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
O�B
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
W
B
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
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
YB
ZB
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
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
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
e`B
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
hsB
iyB
iyB
iyB
jB
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�+B�2B�$B�B�$B�B�B�*B�B�B�B�>B�tB̘B̘B�!B�:B�8B�QB�]B�B�vB�B��B��B��B��B��B��B��B �B�BByB$�B&�B3BI�Be,BoiB��B�BlWB[�BV�BW�BX�B[�Bc:Bh>BjKBq�Bb4B�B�,B��B�RB*�B�oB�B�iB�oB�B��B��B��B�|B�KB)BaB>BBB[BEmBCaB5B%�B%�B/�B"�B!�B�BeB�B[B�B��B��B��B��B�|B��B�HB��B�~B�eB��B��B�<B~�Bu�BkQBQ�B=<B.�B_BB �B
�iB
�&B
бB
�tB
�BB
��B
��B
s�B
g8B
\�B
M�B
1�B
+�B
%�B
xB	��B	� B	ɆB	��B	��B	�xB	��B	qvB	\�B	I�B	,�B	&�B	�B	_B	�B��B�B�,B�B��BΥB�HB��B��B��B��B��B�eB�4B�YB�SB�(B�B��B��B��B��B�~B�qB�kB�eB�eB�]B�eB�9B�4B�B�B��B��B}�B{�Bw�BtnBr|BoOBmCBi*Bf2Bd&BbB^BY�BW�BU�BT�BR�BQ�BQ�BP�BO�BN�BM�BL�BIlBFtBEmBEmBDgBCGBB[BA;B?HB>(B>(B<6B;0B:*B8B6B4�B1�B/�B-�B,�B+�B+�B*�B)�B(�B'�B'�B&�B%�B$�B&�B$�B%�B$�B$�B#�B#�B#�B#�B#�B#�B$�B#�B"�B$�B"�B"�B#�B#�B&�B%�B%�B%�B%�B%�B%�B%�B(�B(�B(�B)�B*�B*�B,�B,�B,�B,�B-�B.�B/�B.�B0�B1�B1�B2�B6B9	B;0B;B;0B=<B?HBCaBDMBGzBK�BM�BO�BR�BU�BW�BY�BY�B]�B_�B`�Bd&BfBh>Bh$BiDBiDBj0BnIBqvBshBtnBw�Bx�By�B{�B}�B~�B�B��B��B��B��B��B��B�B�B�4B�4B�4B�,B�9B�_B�eB�QB�xB�jB��B��B��B��B��B��B��B�B�(B�SB�_BɆBɆBΥBѷB��B��B��B��B�&B�B�DB�=B�OB�aB��B��B��B��B��B	�B	�B	�B	B	.B	@B	EB	kB	xB	�B	�B	 �B	 �B	!�B	#�B	&�B	)�B	+�B	/�B	1�B	2�B	3�B	8B	<B	=<B	?.B	@4B	A;B	CaB	ESB	G_B	J�B	L~B	O�B	R�B	U�B	X�B	Z�B	[�B	\�B	f2B	j0B	k6B	oOB	raB	shB	t�B	v�B	w�B	vzB	x�B	{�B	}�B	��B	��B	��B	�B	�B	� B	�@B	�,B	�MB	�2B	�2B	�FB	�YB	�eB	�]B	�]B	�dB	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�*B	�0B	�"B	�HB	�4B	�;B	�mB	ȀB	�fB	ɆB	�rB	�xB	̈́B	͟B	ΥB	ϫB	ЗB	ѷB	ѝB	ѝB	ңB	ңB	��B	өB	өB	ԯB	յB	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	��B	�B	� B	�&B	�&B	�,B	�2B	�2B	�B	�B	�>B	�$B	�$B	�*B	�KB	�6B	�6B	�WB	�=B	�=B	�=B	�cB	�iB	�iB	�OB	�iB	�iB	�iB	�oB	�UB	�[B	�vB	�aB	�hB	�B	�nB	�nB	��B	�tB	�zB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B

	B

	B

	B

�B

�B
B
B
B
B
"B
B
B
B
B
B
B
.B
.B
B
4B
B
 B
 B
:B
&B
,B
,B
,B
MB
MB
MB
MB
MB
2B
SB
SB
SB
9B
SB
YB
YB
YB
YB
YB
YB
EB
_B
_B
KB
eB
QB
kB
kB
QB
qB
WB
]B
xB
]B
dB
~B
jB
�B
�B
�B
pB
 vB
 vB
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
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
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
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
1�B
1�B
1�B
2�B
2�B
3�B
4B
3�B
5B
4�B
4�B
4�B
4�B
6B
5�B
5�B
5�B
7B
6�B
7B
7B
8B
8B
8B
8B
9	B
9	B
:B
9$B
:B
:*B
;0B
;B
;0B
;B
;B
;B
<B
<6B
<6B
<B
<6B
=<B
="B
>(B
>(B
>BB
>BB
>BB
>(B
>(B
>(B
?.B
?HB
?HB
?HB
?HB
?.B
@4B
@OB
@OB
@4B
A;B
A;B
A;B
AUB
B[B
BAB
B[B
B[B
B[B
CGB
CGB
CaB
CaB
CGB
DMB
DgB
DMB
DgB
DgB
ESB
EmB
EmB
FYB
FtB
FYB
FtB
FYB
GzB
G_B
GzB
G_B
HfB
H�B
HfB
I�B
I�B
IlB
JrB
JrB
J�B
JrB
JrB
JrB
J�B
KxB
KxB
K�B
K�B
L~B
L�B
L~B
L�B
L�B
L~B
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
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
V�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
Y�B
X�B
Y�B
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
\�B
\�B
\�B
]�B
^B
]�B
^B
^�B
^�B
_�B
_�B
`B
`B
`B
`�B
aB
a�B
bB
bB
a�B
bB
bB
a�B
c B
cB
cB
c B
d&B
dB
d&B
d&B
eB
e,B
eB
eB
e,B
e,B
eB
e,B
f2B
fB
fB
fB
fB
f2B
gB
g8B
gB
g8B
gB
g8B
gB
h$B
h>B
h$B
h$B
h>B
h>B
h$B
iDB
i*B
iDB
j0B
j0B
jKB
jKB
j0B
j0B
kQB
j0B
kQB
k6B
k6B
lWB
l=B
l=B
lWB
lWB
l=B
lWB
l=B
lWB
lW111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.51(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901260037312019012600373120190126003731201901270034352019012700343520190127003435JA  ARFMdecpA19c                                                                20190121093701  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190121003717  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190121003719  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190121003719  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190121003720  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190121003720  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190121003720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190121003720  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190121003720  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190121003720                      G�O�G�O�G�O�                JA  ARUP                                                                        20190121005729                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190121153618  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190125153731  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190125153731  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190126153435  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                