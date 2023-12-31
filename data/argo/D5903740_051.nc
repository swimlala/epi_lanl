CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:31Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230831  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               3A   AO  4055_7112_051                   2C  D   APEX                            5374                            041511                          846 @֭mؿ  1   @֭ng(�@9Õ�$��c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    3A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�fD�9�D�� D���D�3D�C3D��3D�ٚD�3D�FfD�vfD��fD��3D�<�Dڙ�D��3D�3D�<�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  B  B	  B  B  B!  B)  B1  B9ffBAffBI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZY�C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� De De� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt Dt� Dt�fDy��D�fD�A�D�� D���D�3D�K3D��3D��D�3D�NfD�~fD��fD��3D�D�Dڡ�D��3D�3D�D�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A��mA��TA̓uA�;dAʥ�A��/AɼjA��A�oA���AȋDA�XA�&�A�
=A�t�A���A��A�%A���A���A��yA�(�A�+A�oA��hA�1A�A�A���A�?}A�?}A���A�XA���A���A�?}A��!A�x�A�=qA���A�ffA�JA��9A���A��!A��\A���A��A�?}A���A�VA�E�A�JA��\A�1'A��A��/A��TA��+A��^A�v�A�7LA��A��`A�jA��`A���A���A��9A�v�A�hsA�
=A�O�A��`A� �A�7LA�l�A�C�A���A��A��PA���A��`A�A�  A���A��A�"�A�$�A�
=A�+A��PA���A���A�{A~�A{7LAxM�AuC�As�-ApI�Am�
Al^5AiAfAb�yA`5?A\�DA[G�AX�RAW�7AV �AT��AT-AS��AR5?AQ&�AP��AN�`AN^5AMAL9XAJE�AIhsAH�HAHAGAFjAEoAC|�AB�yAB�DA@��A@A�A?��A>ffA>1'A>JA=�A<��A;��A9�#A9l�A8��A7�FA6z�A6bA5��A5|�A4�RA4�A3hsA2~�A0�jA/�hA.��A-�mA,�A,M�A*��A)��A)G�A(�/A'�A&�HA%�;A%t�A$n�A#�FA"5?A!��A!O�A �uAG�A��A��AbNA�A�`A?}A�DAI�A��AG�A��A�RAI�A�7AM�A�A�A�jA��A�jAjA$�A�#A��A�A-AC�A
  A	XA��A1A|�A��AA�AJA�A$�A��A%A �!A A�@�K�@�Z@�C�@�ff@��@�(�@�t�@�;d@��y@��\@��@�b@�9@�S�@�M�@���@�9@���@���@蛦@�@�~�@��`@�1@㝲@�^5@��@ߝ�@�n�@݁@��@��;@�5?@�A�@�
=@�E�@���@ӕ�@���@�V@ѡ�@���@�C�@��@�`B@�Q�@���@�;d@�ff@�J@�l�@��@�@��@���@��/@Ý�@��#@��@�o@�M�@���@� �@��@�$�@�@���@��
@�=q@�`B@��D@���@��^@�Ĝ@�r�@�(�@��@�+@��H@�-@�p�@���@�1'@�|�@�E�@���@�/@���@���@�33@��y@�ff@�5?@�@���@�/@�%@�Ĝ@� �@��F@�S�@�
=@��y@�ff@��^@���@���@�dZ@��y@��!@�v�@�E�@���@��h@�&�@���@�Z@���@�C�@��\@�n�@�M�@���@��h@�%@� �@�K�@���@���@�ff@�{@���@��@�7L@��9@�1'@��
@�o@�n�@�M�@�=q@��@��@�X@�/@���@��`@�Ĝ@��@��D@� �@���@���@��F@�|�@�K�@�ȴ@�M�@�J@��^@��@�hs@�7L@��@��9@�9X@��@���@���@�K�@��@��!@�5?@�@��-@��7@�p�@�V@���@�j@�A�@� �@��m@��w@��@�dZ@�K�@�K�@�C�@�C�@�+@��H@���@�M�@���@���@���@�x�@�V@��j@���@��@�j@�Z@�Q�@�A�@���@��@��@���@���@���@��P@�|�@�\)@�+@�
=@��@���@�V@�@��-@�G�@�%@��@��/@���@���@��j@��9@���@�j@�1'@�(�@��@l�@
=@~�y@~ff@}��@}p�@}?}@}?}@|�/@|�@{�
@{33@{@z�H@z�!@z�@y��@y�@x��@xbN@x  @w�w@w�P@w;d@v�@v�+@vv�@vv�@vE�@t�/@q��@i�7@bn�@[�
@Q7L@J�!@DZ@?l�@8��@333@-/@( �@"n�@�@�#@�/@�#@5?@
�!@�@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A��mA��TA̓uA�;dAʥ�A��/AɼjA��A�oA���AȋDA�XA�&�A�
=A�t�A���A��A�%A���A���A��yA�(�A�+A�oA��hA�1A�A�A���A�?}A�?}A���A�XA���A���A�?}A��!A�x�A�=qA���A�ffA�JA��9A���A��!A��\A���A��A�?}A���A�VA�E�A�JA��\A�1'A��A��/A��TA��+A��^A�v�A�7LA��A��`A�jA��`A���A���A��9A�v�A�hsA�
=A�O�A��`A� �A�7LA�l�A�C�A���A��A��PA���A��`A�A�  A���A��A�"�A�$�A�
=A�+A��PA���A���A�{A~�A{7LAxM�AuC�As�-ApI�Am�
Al^5AiAfAb�yA`5?A\�DA[G�AX�RAW�7AV �AT��AT-AS��AR5?AQ&�AP��AN�`AN^5AMAL9XAJE�AIhsAH�HAHAGAFjAEoAC|�AB�yAB�DA@��A@A�A?��A>ffA>1'A>JA=�A<��A;��A9�#A9l�A8��A7�FA6z�A6bA5��A5|�A4�RA4�A3hsA2~�A0�jA/�hA.��A-�mA,�A,M�A*��A)��A)G�A(�/A'�A&�HA%�;A%t�A$n�A#�FA"5?A!��A!O�A �uAG�A��A��AbNA�A�`A?}A�DAI�A��AG�A��A�RAI�A�7AM�A�A�A�jA��A�jAjA$�A�#A��A�A-AC�A
  A	XA��A1A|�A��AA�AJA�A$�A��A%A �!A A�@�K�@�Z@�C�@�ff@��@�(�@�t�@�;d@��y@��\@��@�b@�9@�S�@�M�@���@�9@���@���@蛦@�@�~�@��`@�1@㝲@�^5@��@ߝ�@�n�@݁@��@��;@�5?@�A�@�
=@�E�@���@ӕ�@���@�V@ѡ�@���@�C�@��@�`B@�Q�@���@�;d@�ff@�J@�l�@��@�@��@���@��/@Ý�@��#@��@�o@�M�@���@� �@��@�$�@�@���@��
@�=q@�`B@��D@���@��^@�Ĝ@�r�@�(�@��@�+@��H@�-@�p�@���@�1'@�|�@�E�@���@�/@���@���@�33@��y@�ff@�5?@�@���@�/@�%@�Ĝ@� �@��F@�S�@�
=@��y@�ff@��^@���@���@�dZ@��y@��!@�v�@�E�@���@��h@�&�@���@�Z@���@�C�@��\@�n�@�M�@���@��h@�%@� �@�K�@���@���@�ff@�{@���@��@�7L@��9@�1'@��
@�o@�n�@�M�@�=q@��@��@�X@�/@���@��`@�Ĝ@��@��D@� �@���@���@��F@�|�@�K�@�ȴ@�M�@�J@��^@��@�hs@�7L@��@��9@�9X@��@���@���@�K�@��@��!@�5?@�@��-@��7@�p�@�V@���@�j@�A�@� �@��m@��w@��@�dZ@�K�@�K�@�C�@�C�@�+@��H@���@�M�@���@���@���@�x�@�V@��j@���@��@�j@�Z@�Q�@�A�@���@��@��@���@���@���@��P@�|�@�\)@�+@�
=@��@���@�V@�@��-@�G�@�%@��@��/@���@���@��j@��9@���@�j@�1'@�(�@��@l�@
=@~�y@~ff@}��@}p�@}?}@}?}@|�/@|�@{�
@{33@{@z�H@z�!@z�@y��@y�@x��@xbN@x  @w�w@w�P@w;d@v�@v�+@vv�@vv�@vE�G�O�@q��@i�7@bn�@[�
@Q7L@J�!@DZ@?l�@8��@333@-/@( �@"n�@�@�#@�/@�#@5?@
�!@�@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB�BB�BC�BB�BB�BB�BB�BB�BA�B@�B=qB-B�BoB+BPB-BC�B@�B�B��B�yB��B��B�B[#BO�BS�BO�BI�BD�Bl�By�Bx�Bq�Bk�BcTB[#BR�BdZBdZB`BB^5BS�BL�BJ�BG�BA�B<jB7LB49B?}B?}B<jB49B0!B(�B�B�B�B�BuBVBDB
=BhBB�B�B�B�B�ZB��BǮB�}B�B�hB�JB�=B�Bp�BM�B8RB#�B��BƨB�\Bl�BS�B>wB.B�B%B
�B
�5B
��B
�FB
�bB
p�B
cTB
S�B
C�B
6FB
!�B

=B	�B	�/B	��B	�9B	��B	��B	� B	ffB	O�B	>wB	+B	!�B	uB	PB��B	B��B��B�B�B�B�ZB�HB�/B�B��B��BǮBĜB��B�wB�XB�dB�}B�qB�dB�wB�qB�wB�qB�dB�^B�RB�?B�3B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�DB�1B�+B�%B�B�%B�B�B� B~�B}�B� B~�B}�B{�Bz�Bx�Bv�Bu�Bs�Bo�BjBhsBffBe`B^5BR�BN�BM�BK�BC�B>wB;dB8RB5?B49B33B2-B49B8RB5?B1'B,B'�B%�B#�B!�B�B�B�B�B�B{BuBoBhBbB\BPBVBJBJBVBVBVBVB\B\BVBJB	7B+B	7B	7B1B%B+B1B+B+B%B%B%BB%BB%B%BBB%B	7BDBDBDBPBVBPBPBVB\BVBbBhBhBoBuBhB�B�B�B�B�B�B�B�B�B!�B"�B!�B%�B'�B+B)�B,B.B1'B49B5?B6FB;dB>wB?}B?}B@�BA�BA�BC�BE�BG�BI�BK�BP�BQ�BS�BVBZB]/B^5B`BB`BBaHBaHBdZBe`Be`BhsBjBk�Bm�Bm�Bn�Bp�Bv�Bz�B{�B~�B� B�B�B�B�B�+B�=B�=B�PB�bB�{B�{B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�?B�XB�dB��BÖBĜBĜBǮB��B��B��B��B��B��B��B��B��B�
B�B�/B�;B�HB�TB�fB�B�B�B�B�B��B��B��B��B	  B	B	B	B		7B	JB	PB	bB	hB	oB	�B	�B	�B	�B	 �B	"�B	$�B	'�B	(�B	)�B	)�B	)�B	)�B	+B	.B	/B	2-B	49B	5?B	6FB	8RB	=qB	A�B	B�B	C�B	D�B	E�B	E�B	F�B	K�B	L�B	M�B	M�B	M�B	M�B	N�B	O�B	P�B	R�B	T�B	VB	YB	\)B	^5B	bNB	e`B	gmB	hsB	iyB	iyB	jB	k�B	k�B	l�B	n�B	q�B	r�B	t�B	v�B	w�B	x�B	y�B	{�B	}�B	~�B	~�B	�B	�B	�%B	�=B	�DB	�JB	�JB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�-B	��B	�`B	��B

=B
�B
�B
&�B
0!B
9XB
A�B
I�B
Q�B
VB
[#B
bNB
gmB
k�B
p�B
s�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BB�BB�BC�BB�BB�BB�BB�BB�BA�B@{B=iB-B�BcB BFB-BC�B@|B�B��B�kB��B��B�B[BO�BS�BO�BI�BD�BlzBy�Bx�Bq�BksBcBB[BR�BdGBdHB`/B^!BS�BL�BJ�BG�BAvB<XB76B4%B?jB?gB<XB4$B0B(�B�B�B�B�BaBAB-B
(BPBB�B�kB�wB�tB�GB��BǛB�hB�B�RB�6B�'B��Bp�BM�B8<B#�B��BƑB�DBluBS�B>_B-�B�BB
�zB
�B
ˮB
�0B
�LB
p�B
cAB
S�B
C�B
62B
!�B

+B	�B	�B	̽B	�&B	��B	�qB	�B	fWB	O�B	>kB	*�B	!�B	jB	EB��B	B��B��B�B�B�|B�PB�?B�&B��B��BʸBǥBĕB��B�oB�NB�]B�uB�hB�\B�nB�hB�nB�hB�\B�XB�IB�7B�*B�%B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�PB�<B�+B�'B�B�B�B�B�B�B~�B}�B�B~�B}�B{�Bz�Bx�Bv�Bu�Bs�Bo�BjyBhoBf`BeYB^/BR�BN�BM�BK�BC�B>uB;`B8LB5:B45B3B2B45B8NB5;B1 B,B'�B%�B#�B!�B�B�B�BrBjB^BqBQBJBDBVB0B8B)B+B7B6B8B9BYBXB6BEB	B
B	B	3BBBBBB
BBBB�BBB!BB�B�B B	4B>B>B>B.B5BIBNB6BVBQBCBGBEBPBpBIB�B�B�B�B�B�B�B�B�B!�B"�B!�B%�B'�B*�B)�B+�B.B1 B40B57B6=B;ZB>nB?uB?tB@|BA�BABC�BE�BG�BI�BK�BP�BQ�BS�BU�BZB]$B^*B`8B`7Ba=Ba@BdNBeVBeVBhhBjuBk|Bm�Bm�Bn�Bp�Bv�Bz�B{�B~�B�B��B��B�B�B�B�0B�/B�EB�UB�mB�nB�tB�vB��B��B��B��B��B��B��B�B�B�B�B�/B�IB�VB�rBÈBďBċBǝBʱB˷B̿B��B��B��B��B��B��B��B�B�B�)B�5B�DB�UB�nB�B�B�B�B��B��B��B��B��B	�B	B	B		%B	7B	=B	OB	WB	ZB	tB	�B	�B	�B	 �B	"�B	$�B	'�B	(�B	)�B	)�B	)�B	)�B	*�B	-�B	/B	2B	4$B	5)B	61B	8<B	=ZB	AvB	B|B	C�B	D�B	E�B	E�B	F�B	K�B	L�B	M�B	M�B	M�B	M�B	N�B	O�B	P�B	R�B	T�B	U�B	YB	\B	^ B	b8B	eHB	gWB	h]B	idB	ibB	jiB	knB	knB	lvB	n�B	q�B	r�B	t�B	v�B	w�B	x�B	y�B	{�B	}�B	~�B	~�B	��B	�B	�B	�%B	�/B	�2B	�2B	�CB	�UB	�dB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B	��B	�HB	��B

#B
eB
�B
&�B
0B
9:B
ApB
I�B
Q�B
U�B
[B
b/B
gQB
kgB
p�B
s�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230831    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230831  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230831  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                