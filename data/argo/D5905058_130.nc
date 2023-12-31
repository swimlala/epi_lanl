CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-03-08T21:36:39Z creation;2019-03-08T21:36:42Z conversion to V3.1;2019-12-23T06:05:47Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190308213639  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_130                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ح1�T� 1   @ح2���@8e�S����c4)�y��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Z=q@�Q�@�Q�A(�A&�\AF�\Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&h�C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�4{C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�MD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�PRDRD��RD�RD�PRDÐRD��RD�RD�PRDĐRD��RD�RD�PRDŐRD��RD�RD�PRDƐRD��RD�RD�PRDǐRD��RD�RD�PRDȐRD��RD�RD�PRDɐRD��RD�RD�PRDʐRD��RD�RD�PRDːRD��RD�RD�PRD̐RD��RD�RD�PRD͐RD��RD�RD�PRDΐRD��RD�RD�PRDϐRD��RD�RD�PRDАRD��RD�RD�PRDѐRD��RD�RD�PRDҐRD��RD�RD�PRDӐRD��RD�RD�PRDԐRD��RD�RD�PRDՐRD��RD�RD�PRD֐RD��RD�RD�PRDאRD��RD�RD�PRDؐRD��RD�RD�PRDِRD��RD�RD�PRDڐRD��RD�RD�PRDېRD��RD�RD�PRDܐRD��RD�RD�PRDݐRD��RD�RD�PRDސRD��RD�RD�PRDߐRD��RD�RD�PRD��RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD��D�PRD�RD��RD�RD�PRD瓅D��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD�D��RD�RD�PRD�RD��RD�RD�PRD�RD��RD�RD�PRD��RD��RD�RD�PRD��RD��RD�RD�S�D��RD�ӅD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�?}A�A�A�?}A�C�A�E�A�G�A�G�A�I�A�C�A�G�A�C�A�A�A�A�A�E�A�E�A�C�A�A�A�A�A�C�A�C�A�E�A�C�A�?}A�A�A�C�A�C�A�C�A�E�A�I�A�I�A�I�A�I�A�VA��uA�E�A�$�A��A��!A��7A�?}A�^5A��wA�+A�/A��9A�9XA��#A�p�A�JA��mA���A�p�A�S�A��A�jA���A��`A��^A�  A��
A��uA�I�A���A�/A��jA��9A�7LA�E�A��A�M�A�7LA���A��9A�l�A��DA�bA�$�A�l�A��A���A�|�A�/A��`A�?}A�A�x�A��FA��9A���A��A�;dA�$�A���A�r�A�/A���A�ffA�33A���A���A�E�A�XA�I�A��A}K�Az{Aw?}Atn�As�AqƨAo�wAmAkƨAhVAgS�Af�Ae\)Ad��Adn�AdQ�Ac�Ab9XAa%A^��A\�uA[?}AZjAY�wAYO�AX��AWAT�AQhsAO�ALbNAJ�/AH(�AF�DAD{AA�-A@jA?�PA>��A>^5A=��A=\)A=?}A="�A<�A;+A:bA8{A5��A4^5A3oA1�wA1;dA09XA.v�A-`BA,�\A+�A*�9A)�wA)XA)%A(M�A't�A&��A%�A$�`A$r�A#"�A!��A!&�A ��A �DA+A�AffAG�A%A�DA��A`BA�\A=qA�^A�DA�A��A�A��AƨA��A�-AS�An�A�^A�RAbAoA	��A�jAbNA�A�A&�A�/A�A(�A�FAoAZA/A�DA�A"�A ��A 1@��@��P@���@���@��@���@��
@��@�V@�@��@���@땁@��@�=q@�^@��/@�9X@��@�7L@�F@�;d@�v�@�7L@�ƨ@���@���@�A�@۶F@�M�@�x�@�  @ׅ@��@��@Դ9@�;d@Ұ!@��#@�A�@�K�@�J@�-@�v�@�ff@́@̼j@˝�@���@���@�v�@�7L@�l�@�hs@��j@� �@�~�@��@��@�z�@��H@��@�X@�t�@�@�v�@���@�A�@�C�@�v�@��T@�`B@���@�Q�@��m@�\)@���@�J@��@�V@��j@�A�@�t�@���@��R@�=q@��@��-@��@���@�j@��m@��P@�33@��H@�v�@��@�@���@���@�X@�7L@��@��@�z�@�(�@�b@��;@��F@�\)@�@��@��y@�ȴ@��\@�n�@��@���@�G�@���@��u@�z�@�Z@���@��w@���@�|�@�K�@��R@�ff@�E�@��@���@��^@��@�O�@�Ĝ@�r�@��@��w@�t�@���@�-@��-@�x�@�&�@�Ĝ@��D@�z�@�r�@�I�@�1'@��@��
@���@��w@���@�;d@��@��@���@�n�@�E�@�-@��@���@���@�x�@�O�@�/@��@��/@��D@�bN@�9X@��@��@�\)@�C�@��R@���@��+@�V@��@��h@�?}@�V@��j@���@��@�Z@�I�@��@��
@���@�S�@�+@��@��!@��\@�n�@�=q@�J@��#@���@���@�x�@�O�@�7L@���@���@��D@�z�@�r�@�bN@�A�@� �@� �@�Z@���@���@�z�@�Z@�I�@��@��w@���@�S�@�33@��y@�ff@�-@�E�@���@���@���@���@�p�@�p�@�O�@�/@��@�V@��9@��@�Z@�9X@�1@���@��@�|�@�l�@��@�ȴ@��!@�v�@�^5@�$�@�J@��@��-@��h@�`B@���@��@���@��@�Q�@��@��m@�l�@�33@�
=@��y@�ȴ@���@�~�@�n�@�E�@��@�@��T@���@��h@��@�O�@��@��u@��D@�Q�@�1@|�@~�y@~V@~@}��@}p�@|��@{�m@{t�@{33@z��@z=q@y��@y��@y7L@x��@xr�@xA�@x �@w��@w�@w��@w\)@w�@v�R@vff@v$�@u�@u��@up�@u?}@t�j@t(�@s�F@s"�@r��@r^5@r^5@rM�@r=q@r-@q�#@q�^@qX@p�@pb@o��@o�P@oK�@n��@n�+@nv�@nff@m�@m�-@m��@m�h@m�@l�@lz�@l(�@kdZ@k"�@k@j��@j^5@jJ@i�@i��@i�^@i��@i�7@ihs@hĜ@h1'@g�w@gl�@gK�@g+@f�@f��@f�+@fE�@e��@e/@d��@dz�@dj@dZ@c��@c�@cC�@c@b�H@b��@b�\@bn�@b-@a�#@a�^@a�7@ax�@aX@a7L@a&�@a�@`��@`�9@`�u@`�@`Q�@`  @_�@_l�@_;d@^ff@^$�@^$�@^$�@]�T@]�-@]��@]p�@]/@]V@\��@\�/@\z�@\(�@[�m@[�@[o@Z^5@ZJ@Y��@Y��@Y�@X��@XQ�@XA�@Xb@W��@W+@V��@V5?@V@Up�@T��@T�@Tj@T1@Sƨ@S�@R�H@R~�@R^5@R-@Q��@Qhs@Q7L@P��@PbN@P  @O��@Ol�@OK�@N��@Nȴ@Nff@N@M��@M�-@M�h@M�@M?}@L�@L�D@L�@L1@K��@K�@KC�@K"�@J�H@J~�@J=q@I��@I�#@I�7@H��@HA�@G�@Gl�@G+@F�R@Fv�@F$�@E`B@D�/@DZ@D1@C�F@C�@C"�@B��@B~�@B-@A�#@A��@A�@@��@@�u@@�u@@Q�@@b@?�w@?K�@?;d@>�@>E�@=p�@<�/@<z�@<I�@<I�@<1@;dZ@;o@:��@:��@:^5@:J@9��@9�^@9�7@9X@8��@8Ĝ@8��@8Q�@8b@7�@7K�@7K�@7;d@7�@6�y@6�R@6v�@6V@5��@5��@5�h@5`B@5`B@5/@5V@4�/@4z�@4z�@4I�@4�@41@3�
@3t�@3C�@3o@2�H@2�!@2=q@1��@0�`@0��@0A�@/�P@/l�@/K�@.��@.��@.�+@.E�@.{@-�@-�T@-p�@-/@,�@,�j@,�@,��@,j@+��@+�F@+S�@+"�@*�\@*^5@*-@*-@*=q@*-@)��@)��@)��@)x�@)&�@(��@(�u@(Q�@(  @'�P@';d@';d@'�@&�@&v�@&5?@&@%�-@%��@%��@%�@%`B@%�@$�@$�/@$�j@$�@$z�@$Z@$9X@$�@#ƨ@#�@#"�@"�H@"�!@"�\@"^5@"-@"J@!�#@!��@!��@!X@!G�@ ��@ �`@ �9@ ��@ r�@ bN@ Q�@  �@�@��@l�@;d@�@
=@�y@��@ff@E�@$�@@�@��@�-@p�@O�@?}@�@z�@�m@�F@�@33@�@��@�!@��@��@�\@�\@n�@-@��@%@�`@��@Ĝ@�9@�@r�@Q�@ �@�w@|�@K�@
=@�R@v�@5?@{@{@@@?}@�@V@��@�@��@z�@Z@9X@1@�m@��@t�@dZ@S�@S�@C�@C�@C�@33@"�@"�@@�H@��@n�@=q@��@��@��@�7@X@%@��@�9@Q�@ �@|�@+@��@ȴ@�+@ff@V@E�@5?@$�@{@@�@�T@�T@��@�h@O�@�@�@�/@�@�D@j@9X@9X@1@ƨ@ƨ@�F@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�?}A�A�A�?}A�C�A�E�A�G�A�G�A�I�A�C�A�G�A�C�A�A�A�A�A�E�A�E�A�C�A�A�A�A�A�C�A�C�A�E�A�C�A�?}A�A�A�C�A�C�A�C�A�E�A�I�A�I�A�I�A�I�A�VA��uA�E�A�$�A��A��!A��7A�?}A�^5A��wA�+A�/A��9A�9XA��#A�p�A�JA��mA���A�p�A�S�A��A�jA���A��`A��^A�  A��
A��uA�I�A���A�/A��jA��9A�7LA�E�A��A�M�A�7LA���A��9A�l�A��DA�bA�$�A�l�A��A���A�|�A�/A��`A�?}A�A�x�A��FA��9A���A��A�;dA�$�A���A�r�A�/A���A�ffA�33A���A���A�E�A�XA�I�A��A}K�Az{Aw?}Atn�As�AqƨAo�wAmAkƨAhVAgS�Af�Ae\)Ad��Adn�AdQ�Ac�Ab9XAa%A^��A\�uA[?}AZjAY�wAYO�AX��AWAT�AQhsAO�ALbNAJ�/AH(�AF�DAD{AA�-A@jA?�PA>��A>^5A=��A=\)A=?}A="�A<�A;+A:bA8{A5��A4^5A3oA1�wA1;dA09XA.v�A-`BA,�\A+�A*�9A)�wA)XA)%A(M�A't�A&��A%�A$�`A$r�A#"�A!��A!&�A ��A �DA+A�AffAG�A%A�DA��A`BA�\A=qA�^A�DA�A��A�A��AƨA��A�-AS�An�A�^A�RAbAoA	��A�jAbNA�A�A&�A�/A�A(�A�FAoAZA/A�DA�A"�A ��A 1@��@��P@���@���@��@���@��
@��@�V@�@��@���@땁@��@�=q@�^@��/@�9X@��@�7L@�F@�;d@�v�@�7L@�ƨ@���@���@�A�@۶F@�M�@�x�@�  @ׅ@��@��@Դ9@�;d@Ұ!@��#@�A�@�K�@�J@�-@�v�@�ff@́@̼j@˝�@���@���@�v�@�7L@�l�@�hs@��j@� �@�~�@��@��@�z�@��H@��@�X@�t�@�@�v�@���@�A�@�C�@�v�@��T@�`B@���@�Q�@��m@�\)@���@�J@��@�V@��j@�A�@�t�@���@��R@�=q@��@��-@��@���@�j@��m@��P@�33@��H@�v�@��@�@���@���@�X@�7L@��@��@�z�@�(�@�b@��;@��F@�\)@�@��@��y@�ȴ@��\@�n�@��@���@�G�@���@��u@�z�@�Z@���@��w@���@�|�@�K�@��R@�ff@�E�@��@���@��^@��@�O�@�Ĝ@�r�@��@��w@�t�@���@�-@��-@�x�@�&�@�Ĝ@��D@�z�@�r�@�I�@�1'@��@��
@���@��w@���@�;d@��@��@���@�n�@�E�@�-@��@���@���@�x�@�O�@�/@��@��/@��D@�bN@�9X@��@��@�\)@�C�@��R@���@��+@�V@��@��h@�?}@�V@��j@���@��@�Z@�I�@��@��
@���@�S�@�+@��@��!@��\@�n�@�=q@�J@��#@���@���@�x�@�O�@�7L@���@���@��D@�z�@�r�@�bN@�A�@� �@� �@�Z@���@���@�z�@�Z@�I�@��@��w@���@�S�@�33@��y@�ff@�-@�E�@���@���@���@���@�p�@�p�@�O�@�/@��@�V@��9@��@�Z@�9X@�1@���@��@�|�@�l�@��@�ȴ@��!@�v�@�^5@�$�@�J@��@��-@��h@�`B@���@��@���@��@�Q�@��@��m@�l�@�33@�
=@��y@�ȴ@���@�~�@�n�@�E�@��@�@��T@���@��h@��@�O�@��@��u@��D@�Q�@�1@|�@~�y@~V@~@}��@}p�@|��@{�m@{t�@{33@z��@z=q@y��@y��@y7L@x��@xr�@xA�@x �@w��@w�@w��@w\)@w�@v�R@vff@v$�@u�@u��@up�@u?}@t�j@t(�@s�F@s"�@r��@r^5@r^5@rM�@r=q@r-@q�#@q�^@qX@p�@pb@o��@o�P@oK�@n��@n�+@nv�@nff@m�@m�-@m��@m�h@m�@l�@lz�@l(�@kdZ@k"�@k@j��@j^5@jJ@i�@i��@i�^@i��@i�7@ihs@hĜ@h1'@g�w@gl�@gK�@g+@f�@f��@f�+@fE�@e��@e/@d��@dz�@dj@dZ@c��@c�@cC�@c@b�H@b��@b�\@bn�@b-@a�#@a�^@a�7@ax�@aX@a7L@a&�@a�@`��@`�9@`�u@`�@`Q�@`  @_�@_l�@_;d@^ff@^$�@^$�@^$�@]�T@]�-@]��@]p�@]/@]V@\��@\�/@\z�@\(�@[�m@[�@[o@Z^5@ZJ@Y��@Y��@Y�@X��@XQ�@XA�@Xb@W��@W+@V��@V5?@V@Up�@T��@T�@Tj@T1@Sƨ@S�@R�H@R~�@R^5@R-@Q��@Qhs@Q7L@P��@PbN@P  @O��@Ol�@OK�@N��@Nȴ@Nff@N@M��@M�-@M�h@M�@M?}@L�@L�D@L�@L1@K��@K�@KC�@K"�@J�H@J~�@J=q@I��@I�#@I�7@H��@HA�@G�@Gl�@G+@F�R@Fv�@F$�@E`B@D�/@DZ@D1@C�F@C�@C"�@B��@B~�@B-@A�#@A��@A�@@��@@�u@@�u@@Q�@@b@?�w@?K�@?;d@>�@>E�@=p�@<�/@<z�@<I�@<I�@<1@;dZ@;o@:��@:��@:^5@:J@9��@9�^@9�7@9X@8��@8Ĝ@8��@8Q�@8b@7�@7K�@7K�@7;d@7�@6�y@6�R@6v�@6V@5��@5��@5�h@5`B@5`B@5/@5V@4�/@4z�@4z�@4I�@4�@41@3�
@3t�@3C�@3o@2�H@2�!@2=q@1��@0�`@0��@0A�@/�P@/l�@/K�@.��@.��@.�+@.E�@.{@-�@-�T@-p�@-/@,�@,�j@,�@,��@,j@+��@+�F@+S�@+"�@*�\@*^5@*-@*-@*=q@*-@)��@)��@)��@)x�@)&�@(��@(�u@(Q�@(  @'�P@';d@';d@'�@&�@&v�@&5?@&@%�-@%��@%��@%�@%`B@%�@$�@$�/@$�j@$�@$z�@$Z@$9X@$�@#ƨ@#�@#"�@"�H@"�!@"�\@"^5@"-@"J@!�#@!��@!��@!X@!G�@ ��@ �`@ �9@ ��@ r�@ bN@ Q�@  �@�@��@l�@;d@�@
=@�y@��@ff@E�@$�@@�@��@�-@p�@O�@?}@�@z�@�m@�F@�@33@�@��@�!@��@��@�\@�\@n�@-@��@%@�`@��@Ĝ@�9@�@r�@Q�@ �@�w@|�@K�@
=@�R@v�@5?@{@{@@@?}@�@V@��@�@��@z�@Z@9X@1@�m@��@t�@dZ@S�@S�@C�@C�@C�@33@"�@"�@@�H@��@n�@=q@��@��@��@�7@X@%@��@�9@Q�@ �@|�@+@��@ȴ@�+@ff@V@E�@5?@$�@{@@�@�T@�T@��@�h@O�@�@�@�/@�@�D@j@9X@9X@1@ƨ@ƨ@�F@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�'B	�'B	�-B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�-B	�?B	��B
u�B
�sB
�sB
�yB
�BB�B#�Bl�Bz�B� B�B�Bo�BS�BP�BQ�BP�BO�BL�B@�B2-B�B=qBffBw�B�JB�B��B��BÖB�)B�B�BB�)B�NB�BBÖB�hB� Bs�B_;B\)B$�B
��B
�B
��B
��B
�B
y�B
�NB6FBK�BB�B,B"�B
=B
�B�BB
�B
�LB
�B
��B
��B
�B
��B
�VB
T�B
D�B
�B	��B	�5B	��B	��B	�'B	��B	��B	�7B	|�B	w�B	r�B	hsB	cTB	aHB	_;B	^5B	W
B	J�B	:^B	2-B	+B	'�B	#�B	 �B	�B	{B	DB��B�B�`B�B��BÖB�jB�B��B��B��B��B��B��B��B��B�bB�=B�By�BffB^5BW
BO�BN�BM�BJ�BE�BD�B?}B>wB;dB:^B9XB:^B:^B9XB8RB=qBF�B=qB<jB=qB=qB?}B=qBG�BH�BF�BVBYBXBW
BS�BR�BP�BL�B@�B8RB5?B2-B0!B1'B49B5?B7LB5?B49B33B1'B.B-B,B,B+B)�B)�B)�B(�B(�B&�B'�B&�B$�B$�B$�B"�B"�B!�B!�B!�B �B �B �B �B�B!�B �B �B!�B"�B"�B!�B"�B"�B"�B"�B$�B$�B$�B$�B$�B'�B'�B)�B,B-B-B.B1'B1'B1'B2-B33B49B33B5?B8RB8RB=qBB�BG�BQ�BW
BXBZBZB\)B\)B^5B^5B]/B^5B_;B\)B]/BdZBdZBe`BcTBdZBjBk�Bn�Br�Bv�By�B|�B� B�B�B�+B�7B�JB�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�FB�^B�jB�qB�wB��BÖBĜBŢBƨBɺB��B��B��B��B��B�B�)B�/B�5B�BB�HB�`B�yB�B�B��B��B��B��B	B	B	B	%B	
=B	JB	VB	\B	hB	{B	�B	�B	�B	�B	#�B	%�B	&�B	+B	.B	0!B	2-B	5?B	8RB	<jB	=qB	@�B	@�B	A�B	C�B	E�B	E�B	F�B	F�B	H�B	I�B	K�B	N�B	P�B	R�B	S�B	W
B	YB	[#B	]/B	`BB	aHB	cTB	cTB	ffB	gmB	hsB	hsB	jB	m�B	q�B	u�B	u�B	u�B	w�B	y�B	}�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�JB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�RB	�^B	�dB	�jB	�jB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
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
\B
\B
\B
bB
hB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
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
/B
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
1'B
1'B
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
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
?}B
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
C�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
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
F�B
G�B
G�B
G�B
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
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
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
R�B
R�B
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
S�B
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
XB
XB
XB
XB
XB
XB
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
ZB
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
\)B
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
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
e`B
e`B
e`B
e`B
e`B
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
jB
jB
jB
jB
jB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	̳B
u�B
�>B
�>B
�DB
�QB�B�B#�BlqBz�B�B��B��BoiBS�BP�BQ�BP�BO�BL�B@OB1�B�B=<BfLBw�B�B��BΥBбB�aB�B��B�'B��B�B�B�aB�4B�Bs�B_B[�B$�B
��B
�vB
͟B
��B
��B
y�B
�B6BK�BB[B+�B"�B
	B
�|BqB�B
��B
�B
��B
��B
��B
��B
��B
�"B
T�B
DgB
_B	��B	�B	ѷB	�OB	��B	��B	�YB	�B	|�B	w�B	r|B	h>B	c B	aB	_B	^B	V�B	J�B	:*B	1�B	*�B	'�B	#�B	 �B	qB	FB	B��B�|B�,B��B͟B�aB�B��B��B��B��B��B�QB�_B�YB�SB�.B�	B��By�Bf2B^BV�BO�BN�BM�BJ�BEmBDgB?HB>BB;0B:*B9$B:*B:*B9$B8B=<BFtB=<B<B=<B=<B?HB=<BGzBH�BFtBU�BX�BW�BV�BS�BR�BP�BL�B@OB8B5B1�B/�B0�B4B4�B6�B4�B4B2�B0�B-�B,�B+�B+�B*�B)�B)�B)�B(�B(�B&�B'�B&�B$�B$�B$�B"�B"�B!�B!|B!�B �B �B �B �B�B!�B vB �B!�B"�B"�B!�B"�B"�B"�B"�B$�B$�B$�B$�B$�B'�B'�B)�B+�B,�B,�B-�B0�B0�B0�B1�B2�B3�B2�B5B8B8B=<BBABGzBQ�BV�BW�BY�BY�B[�B[�B]�B]�B\�B^B_B[�B\�BdBdBe,BcBdBj0BkQBncBraBvzBy�B|�B�B��B��B��B��B��B�B�4B�:B�FB�2B�?B�qB�~B��B��B��B��B��B��B��B��B��B��B�B�*B�B�"B�(B�4B�GB�MB�mB�tB�lB�~B̈́BбBҽB��B��B��B��B��B��B��B�,B�DB�]B�aB�B��B��B��B	 �B	�B	�B	�B	
	B	�B	"B	(B	B	FB	SB	KB	dB	�B	#�B	%�B	&�B	*�B	-�B	/�B	1�B	5B	8B	<6B	="B	@4B	@4B	A;B	CGB	ESB	ESB	FtB	FtB	HfB	IlB	K�B	N�B	P�B	R�B	S�B	V�B	X�B	Z�B	\�B	`B	`�B	cB	cB	fB	g8B	h>B	h>B	j0B	mCB	qvB	utB	utB	utB	w�B	y�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�&B	�2B	�SB	�EB	�kB	�]B	��B	��B	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�0B	�B	�B	�"B	�BB	�HB	�;B	�AB	�aB	�gB	�YB	�lB	�xB	�~B	̈́B	͟B	ΥB	ΊB	ϫB	ϑB	ϫB	ЗB	ңB	өB	��B	ԯB	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�,B	�,B	�B	�2B	�8B	�>B	�DB	�0B	�0B	�6B	�6B	�WB	�=B	�WB	�]B	�CB	�CB	�cB	�IB	�iB	�iB	�OB	�oB	�aB	�|B	�aB	�B	�B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B

	B
	�B
B
B

�B

�B
�B
B
B
�B
B
B
"B
"B
B
B
B
(B
(B
B
B
(B
.B
B
 B
:B
@B
@B
&B
FB
,B
,B
MB
MB
SB
9B
SB
9B
?B
?B
?B
EB
_B
_B
_B
EB
KB
KB
KB
KB
eB
eB
kB
kB
QB
QB
QB
QB
kB
kB
WB
qB
qB
qB
~B
~B
dB
~B
~B
~B
dB
�B
jB
�B
jB
jB
pB
�B
�B
 �B
 vB
!|B
!�B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
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
.�B
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
0�B
0�B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
4B
4B
4�B
5�B
5�B
6B
7B
6�B
7B
8B
8B
8B
8B
9$B
9$B
9	B
9	B
:B
:B
:*B
:B
;0B
;B
<6B
<6B
=<B
="B
="B
?HB
?.B
?.B
?.B
?HB
@4B
@4B
@4B
@4B
@4B
A;B
A;B
A;B
AUB
AUB
B[B
BAB
B[B
BAB
BAB
BAB
BAB
CaB
CGB
CaB
CaB
CGB
DMB
CGB
DgB
DMB
DMB
ESB
EmB
EmB
EmB
ESB
ESB
FYB
FYB
FYB
FYB
FYB
G_B
GzB
G_B
G_B
G_B
G_B
GzB
GzB
HfB
H�B
H�B
H�B
H�B
I�B
IlB
IlB
I�B
I�B
I�B
IlB
JrB
JrB
JrB
JrB
JrB
KxB
K�B
K�B
L~B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
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
R�B
R�B
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
S�B
T�B
T�B
T�B
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
U�B
V�B
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
Y�B
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
[�B
\�B
\�B
\�B
\�B
^B
^B
]�B
_B
^�B
_B
_B
^�B
_B
_B
`B
_�B
`B
`B
_�B
_�B
`B
aB
aB
`�B
bB
bB
a�B
a�B
a�B
bB
bB
a�B
a�B
bB
bB
bB
cB
c B
c B
cB
c B
d&B
d&B
dB
dB
e,B
e,B
eB
eB
eB
fB
f2B
g8B
g8B
g8B
g8B
g8B
gB
gB
gB
h>B
h>B
h$B
h>B
h>B
h>B
h$B
h$B
i*B
i*B
iDB
iDB
i*B
iDB
j0B
j0B
j0B
jKB
j0B
jKB
jK1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.51(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903140036322019031400363220190314003632201903150022592019031500225920190315002259JA  ARFMdecpA19c                                                                20190309063622  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190308213639  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190308213640  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190308213641  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190308213641  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190308213641  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190308213641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190308213641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190308213642  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190308213642                      G�O�G�O�G�O�                JA  ARUP                                                                        20190308215619                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190309153355  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20190313153632  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190313153632  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190314152259  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                