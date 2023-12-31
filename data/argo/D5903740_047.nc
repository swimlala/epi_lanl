CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:29Z AOML 3.0 creation; 2016-06-01T00:08:13Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230829  20160531170813  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               /A   AO  4055_7112_047                   2C  D   APEX                            5374                            041511                          846 @֣*K� 	1   @֣*�P�@:�t��cdZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    /A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy�fD���D�FfD���D�� D�fD�S3D���D��3D�	�D�,�D�y�DǦfD�3D�C3Dډ�D���D�3D�0 D�s3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  A  A$  AD  Ad  A�  A�  A�  A�  A�  A�  A�  A�  BffB	  B  B  B!  B)  B1  B9  BA  BI  BQ  BY  Ba  Bi  Bq  By  B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� B�� BĀ BȀ B̀ BЀ BԀ B؀ B܀ B�� B� B� B� B�� B� B�� B�� C @ C@ C@ C@ C@ C
@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C@ C @ C"@ C$@ C&@ C(@ C*@ C,@ C.@ C0@ C2@ C4@ C6@ C8@ C:@ C<@ C>@ C@@ CB@ CD@ CF@ CH@ CJ@ CL@ CN@ CP@ CR@ CT@ CV@ CX@ CZ@ C\@ C^@ C`@ Cb@ Cd@ Cf@ Ch@ Cj@ Cl@ Cn@ Cp@ Cr@ Ct@ Cv@ Cx@ Cz@ C|@ C~@ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D  D � D D� D D� D D� D D� D D� D D� D D� D D� D	 D	� D
 D
� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D D� D  D � D! D!� D" D"� D# D#� D$ D$� D% D%� D& D&� D' D'� D( D(� D) D)� D* D*� D+ D+� D, D,� D- D-� D. D.� D/ D/� D0 D0� D1 D1� D2 D2� D3 D3� D4 D4� D5 D5� D6 D6� D7 D7� D8 D8� D9 D9� D: D:� D; D;� D< D<� D= D=� D> D>� D? D?� D@ D@� DA DA� DB DB� DC DC� DD DD� DE DE� DF DF� DG DG� DH DH� DI DI� DJ DJ� DK DK� DL DL� DM DM� DN DN� DO DO� DP DP� DQ DQ� DR DR� DS DS� DT DT� DU DU� DV DV� DW DW� DX DX� DY DY� DZ DZ� D[ D[� D\ D\� D] D]� D^ D^� D_ D_� D` D`� Da Da� Db Db� Dc Dc� Dd Dd� DefDe� Df Df� Dg Dg� Dh Dh� Di Di� Dj Dj� Dk Dk� Dl Dl� Dm Dm� Dn Dn� Do Do� Dp Dp� Dq Dq� Dr Dr� Ds Ds� Dt3Dy�fD��D�NfD���D�� D�fD�[3D���D��3D��D�4�D���DǮfD�3D�K3Dڑ�D���D�3D�8 D�{3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��yA��A��A��A��A��A��A��A��yA��yA��yA��A��HA¼jA��/A�$�A�VA�E�A��PA�G�A���A��yA��DA��PA�ƨA�hsA��RA��A���A�bA�jA��A���A��DA�x�A�{A�~�A��!A�M�A���A�M�A��9A�5?A�  A��DA��A���A�I�A���A��A�A�VA��A���A�K�A���A�A��A�G�A��!A�{A�;dA��DA�r�A��HA�"�A��TA��A��A�"�A���A��A��A���A�x�A��A�33A��uA�^5A��A�S�A�^5A���A�jA�`BA��A�v�A�E�A���A�1A���A��9A��A�33A�ffA��wA�C�A}�7A|bNA{�7Az�Ay��Ax$�Au��As�As%Arr�Aq��Aq�Ao��AnVAl��Ak|�Ai��Ah5?AgG�Ae+Ac\)Ab��Aa��A`�A_C�A\��A[VAZ9XAX��AV�jAU�hAShsAQ�AN9XAL^5AKoAJ �AJ�AIG�AH$�AG�wAGAF�`AFZAE�wAD�`AD �AB��AA�#AA&�A@��A@Q�A@�A?�A>��A=�A=XA=�A<�A;l�A:�A9/A7�A6Q�A5��A4�A3�#A1ƨA0��A/O�A.$�A-�hA+�A+&�A*�jA)/A(�9A'�-A'`BA'33A'VA&��A&5?A$~�A$bA#��A#hsA"��A"1'A!�^A!?}A ^5A�
A��A��A�A�A�
AVAVA�
AbNA�yAE�A��Ap�A�TA%A�
A��A�-AJA
��A	�A�/AQ�A�TAp�Az�A��Av�A��A��AA j@��@���@�p�@���@���@�"�@���@��u@�@�X@�ȴ@��#@�`B@��@�P@��@�G�@��;@�@��@睲@�33@�@�A�@���@�@�b@ޏ\@ݙ�@�O�@�Q�@�v�@ؼj@���@�t�@֏\@պ^@�X@��@���@�{@�?}@���@�ƨ@���@�ȴ@·+@�X@��`@�Q�@�o@ə�@�&�@ȓu@Ǿw@�n�@���@���@��;@�\)@��H@�@���@�p�@���@�9X@��
@�S�@�~�@���@��w@�n�@���@�  @�t�@��@�j@��P@�
=@�{@��-@�X@��u@���@�\)@��H@�~�@�^5@�J@�%@�1@�+@��@�G�@��@�j@��
@�33@��-@���@�Z@� �@�C�@��R@�E�@���@��@�t�@�"�@��+@���@���@��D@�b@���@�t�@�o@��+@��@�O�@���@�(�@���@�33@��@�V@�J@�O�@��j@��9@�z�@�A�@��F@�dZ@�o@�ȴ@���@�M�@���@���@��h@�7L@�%@�Ĝ@�Q�@��@��@�|�@�\)@��@��@���@�M�@�@��T@���@�G�@��@���@�I�@�1@��;@���@�t�@�dZ@�S�@�;d@�+@���@�ȴ@���@�n�@��@���@���@��u@�9X@�  @��
@�ƨ@���@��@�;d@�o@�ȴ@�v�@�^5@��@�@���@�`B@��@��j@���@�z�@�bN@�b@��@��;@���@�l�@�;d@�
=@���@��\@���@�v�@�E�@�J@���@���@�x�@�X@�G�@�O�@�G�@�G�@�?}@�&�@�%@���@��@��/@��u@�1'@��@�  @�w@;d@~��@~v�@}�@}��@}p�@}O�@|��@|�@|z�@|9X@{ƨ@{"�@z�H@z��@z~�@zM�@y��@y��@y�7@yG�@x�u@x1'@x  @w�w@w;d@v�@vv�@vff@vff@vE�@u�T@u�-@u�@u?}@r�@j^5@c�F@\j@Tz�@L��@E��@AX@;S�@5@1%@+�F@'
=@!�@O�@��@/@��@�R@	X@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��yA��A��A��A��A��A��A��A��yA��yA��yA��A��HA¼jA��/A�$�A�VA�E�A��PA�G�A���A��yA��DA��PA�ƨA�hsA��RA��A���A�bA�jA��A���A��DA�x�A�{A�~�A��!A�M�A���A�M�A��9A�5?A�  A��DA��A���A�I�A���A��A�A�VA��A���A�K�A���A�A��A�G�A��!A�{A�;dA��DA�r�A��HA�"�A��TA��A��A�"�A���A��A��A���A�x�A��A�33A��uA�^5A��A�S�A�^5A���A�jA�`BA��A�v�A�E�A���A�1A���A��9A��A�33A�ffA��wA�C�A}�7A|bNA{�7Az�Ay��Ax$�Au��As�As%Arr�Aq��Aq�Ao��AnVAl��Ak|�Ai��Ah5?AgG�Ae+Ac\)Ab��Aa��A`�A_C�A\��A[VAZ9XAX��AV�jAU�hAShsAQ�AN9XAL^5AKoAJ �AJ�AIG�AH$�AG�wAGAF�`AFZAE�wAD�`AD �AB��AA�#AA&�A@��A@Q�A@�A?�A>��A=�A=XA=�A<�A;l�A:�A9/A7�A6Q�A5��A4�A3�#A1ƨA0��A/O�A.$�A-�hA+�A+&�A*�jA)/A(�9A'�-A'`BA'33A'VA&��A&5?A$~�A$bA#��A#hsA"��A"1'A!�^A!?}A ^5A�
A��A��A�A�A�
AVAVA�
AbNA�yAE�A��Ap�A�TA%A�
A��A�-AJA
��A	�A�/AQ�A�TAp�Az�A��Av�A��A��AA j@��@���@�p�@���@���@�"�@���@��u@�@�X@�ȴ@��#@�`B@��@�P@��@�G�@��;@�@��@睲@�33@�@�A�@���@�@�b@ޏ\@ݙ�@�O�@�Q�@�v�@ؼj@���@�t�@֏\@պ^@�X@��@���@�{@�?}@���@�ƨ@���@�ȴ@·+@�X@��`@�Q�@�o@ə�@�&�@ȓu@Ǿw@�n�@���@���@��;@�\)@��H@�@���@�p�@���@�9X@��
@�S�@�~�@���@��w@�n�@���@�  @�t�@��@�j@��P@�
=@�{@��-@�X@��u@���@�\)@��H@�~�@�^5@�J@�%@�1@�+@��@�G�@��@�j@��
@�33@��-@���@�Z@� �@�C�@��R@�E�@���@��@�t�@�"�@��+@���@���@��D@�b@���@�t�@�o@��+@��@�O�@���@�(�@���@�33@��@�V@�J@�O�@��j@��9@�z�@�A�@��F@�dZ@�o@�ȴ@���@�M�@���@���@��h@�7L@�%@�Ĝ@�Q�@��@��@�|�@�\)@��@��@���@�M�@�@��T@���@�G�@��@���@�I�@�1@��;@���@�t�@�dZ@�S�@�;d@�+@���@�ȴ@���@�n�@��@���@���@��u@�9X@�  @��
@�ƨ@���@��@�;d@�o@�ȴ@�v�@�^5@��@�@���@�`B@��@��j@���@�z�@�bN@�b@��@��;@���@�l�@�;d@�
=@���@��\@���@�v�@�E�@�J@���@���@�x�@�X@�G�@�O�@�G�@�G�@�?}@�&�@�%@���@��@��/@��u@�1'@��@�  @�w@;d@~��@~v�@}�@}��@}p�@}O�@|��@|�@|z�@|9X@{ƨ@{"�@z�H@z��@z~�@zM�@y��@y��@y�7@yG�@x�u@x1'@x  @w�w@w;d@v�@vv�@vff@vff@vE�@u�T@u�-@u�@u?}@r�@j^5@c�F@\j@Tz�@L��@E��@AX@;S�@5@1%@+�F@'
=@!�@O�@��@/@��@�R@	X@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�DB�7B�\B�bB�\B�\B�PB�\B��B��B�uB�hB��B��B�\B�VB�PB�JB�JB�JB�DB�Bp�BhsB_;BZBQ�BI�BF�BC�B>wB:^B:^B/B'�B�BDB��B�B�ZB�
B�jB��B��B�bB�Bo�B]/BC�B'�B�B%B�B�;BȴB�!B��B��B�=B|�BdZBW
BI�BB�B6FB�BbB1B
��B
�/B
ȴB
�dB
��B
�{B
�1B
�B
�B
w�B
iyB
]/B
P�B
E�B
33B
)�B
#�B
�B
�B

=B	��B	�B	�yB	�`B	�HB	�)B	��B	ɺB	�wB	�FB	��B	��B	��B	�JB	�B	{�B	v�B	q�B	ffB	W
B	O�B	J�B	A�B	8RB	-B	�B	PB��B��B��B�B��B��B�B�B�B�B�B��B��B�B�B�fB�TB�NB�TB�yB�sB�B�yB�B�B�mB�5B��BȴB�wB�?B�!B�B��B��B�oB�bB�JB�1B�%B�B�B� B~�B|�B{�Bz�By�Bx�Bu�Bt�Bt�Bt�Bs�Bq�Bp�Bo�Bn�Bm�Bq�Bw�By�Bt�Bo�Bl�BjBk�BgmB^5BZB\)BW
BN�BG�BC�B>wB9XB49B/B+B(�B&�B%�B$�B"�B �B�B�B�B{BuBoBhBhBhBhBbB\BVBJBDB	7B	7B	7B	7B1B+B+B%B%BBBBBBBBBBB%B%B%B%B+B1B1B	7B
=B	7B
=BJBPBVBVBVB\B\B\BbBhBhBoB�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B"�B$�B&�B(�B,B.B.B2-B6FB8RB9XB<jB<jB=qB?}B@�BA�BC�BD�BD�BD�BG�BI�BL�BO�BP�BQ�BS�BT�BW
B]/BaHBdZBdZBhsBk�Bm�Bp�Bt�By�B{�B}�B�B�B�+B�7B�DB�DB�PB�bB�oB��B��B��B��B��B��B��B��B�B�'B�'B�-B�3B�LB�XB�dB�wB�}BBĜBƨBǮB��B��B��B��B��B�
B�B�#B�/B�;B�BB�ZB�mB�yB�B�B�B�B��B��B��B	  B	B	B	B	+B	1B	
=B	JB	VB	\B	bB	hB	uB	�B	�B	�B	�B	�B	!�B	"�B	$�B	%�B	'�B	)�B	,B	-B	0!B	2-B	33B	6FB	9XB	:^B	:^B	<jB	?}B	A�B	A�B	C�B	H�B	I�B	I�B	I�B	N�B	P�B	S�B	YB	ZB	]/B	aHB	bNB	cTB	dZB	e`B	gmB	gmB	iyB	iyB	jB	k�B	l�B	m�B	o�B	s�B	t�B	u�B	v�B	y�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�PB	�VB	�\B	�\B	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	��B	�TB	��B
1B
�B
"�B
)�B
1'B
9XB
?}B
F�B
K�B
R�B
YB
]/B
bNB
e`B
iyB
p�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�$B�MB�OB�JB�KB�=B�OB�vB�|B�cB�WB�zB�qB�LB�FB�@B�=B�8B�;B�6B��Bp�BhaB_(BZ
BQ�BI�BF�BC�B>dB:EB:IB/B'�B�B.B��B�tB�FB��B�WB��B��B�KB�
Bo�B]BC�B'�B�BB�B�"BȜB�B��B�vB�&B|�BdBBV�BI�BByB6-B�BKBB
��B
�B
ȟB
�NB
��B
�dB
�B
�B
��B
w�B
ieB
]B
P�B
E�B
3B
)�B
#�B
�B
sB

+B	��B	�B	�iB	�PB	�8B	�B	��B	ɪB	�hB	�5B	��B	��B	�B	�:B	��B	{�B	v�B	q�B	fWB	V�B	O�B	J�B	A{B	8EB	-B	�B	FB��B��B��B�B��B��B�B�B�B�B�B��B��B�B�{B�`B�KB�EB�KB�mB�gB�uB�pB�B�|B�cB�-B��BȮB�oB�9B�B��B��B�xB�jB�[B�DB�,B�B�B�B�B~�B|�B{�Bz�By�Bx�Bu�Bt�Bt�Bt�Bs�Bq�Bp�Bo�Bn�Bm�Bq�Bw�By�Bt�Bo�Bl�BjyBk~BgfB^.BZB\$BWBN�BG�BC�B>rB9RB46B/B*�B(�B&�B%�B$�B"�B �B�B�BpB\BUBPBJBIBKBHBCB:B9B,B$B	B	B	B	BBBBBBB�B B BB B�B�B B�BBBBB'BB-B	3B
B	B
BEB1B6B6B5BUBXB>BBBIBGBgBzB}B|BeB�B�B�B�B�B�B�B�B�B �B!�B!�B!�B"�B$�B&�B(�B, B.B.B2&B6=B8KB9QB<bB<bB=jB?sB@{BA~BC�BD�BD�BD�BG�BI�BL�BO�BP�BQ�BS�BT�BW B]&Ba>BdPBdNBhkBkxBm�Bp�Bt�By�B{�B}�B�B�B�B�+B�7B�7B�BB�UB�cB�wB��B��B��B��B��B��B��B��B�B�B�B�$B�;B�EB�UB�gB�mB�~BĎBƖBǝBʱB˶B̽B��B��B��B�B�B�B�*B�0B�HB�\B�iB�uB�B�B�B��B��B��B��B	�B	�B	B	B	B	
)B	7B	EB	JB	QB	WB	bB	{B	�B	�B	�B	�B	!�B	"�B	$�B	%�B	'�B	)�B	+�B	,�B	0B	2B	3B	6/B	9CB	:IB	:JB	<RB	?iB	AuB	AtB	C�B	H�B	I�B	I�B	I�B	N�B	P�B	S�B	YB	ZB	]B	a3B	b8B	c>B	dEB	eKB	gUB	gWB	ibB	icB	jhB	krB	ltB	m|B	o�B	s�B	t�B	u�B	v�B	y�B	{�B	|�B	~�B	�B	��B	��B	��B	�B	�B	�B	�B	�1B	�8B	�?B	�BB	�BB	�WB	�]B	�]B	�fB	�uB	�|B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ͺB	�:B	��B
B
nB
"�B
)�B
1B
9=B
?`B
F�B
K�B
R�B
X�B
]B
b4B
eDB
i\B
p�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.25 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708132016053117081320160531170813  AO  ARCAADJP                                                                    20140721230829    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230829  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230829  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170813  IP                  G�O�G�O�G�O�                