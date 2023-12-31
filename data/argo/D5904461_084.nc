CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-13T13:46:13Z AOML 3.0 creation; 2016-08-09T21:33:54Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160413134613  20160809143354  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               TA   AO  5286_8897_084                   2C  D   APEX                            6531                            072314                          846 @��T��1   @���>��@3.z�G��c�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    TA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyffD��D�FfD��fD���D��D�VfD���D�y�D�  D�9�D�� DǼ�D��D�L�D�vfD�3D� D�I�D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BPp�BXp�B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�8RB�8RB���B�B�B�8RB���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C)C )C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �DtmqDyg
D��D�F�D���D��D��D�V�D���D�y�D� RD�9�D��RDǽD�D�MD�v�D೅D�RD�I�D�}D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A��A��A��A��yA��A��TA���A���A���A���A���A���A���A���A���A�ƨA�A���A���A���A���A���A���A�A�A�A�ȴA���A���A���A��#A���A���A�ĜA�M�A�S�A�VA��yAȍPA��#A�~�Aś�A�5?A�{A�`BA�l�A�z�A���A�S�A���A�ZA���A�A�v�A��-A���A���A���A�1A��A��;A��A� �A��yA���A��A��A��A�A�l�A�^5A��A��A�\)A��jA�1'A�ffA��jA�A�/A��A���A���A��A���A�JA�x�A�(�A��A���A�oA���A�r�A�$�A�Q�A��A��A�=qA�{A�
A{oAy�Ax��Aw33At��At��At�9At��AsƨAp��Al�HAj�Af(�Aa%A]��A[�mAZ{AY�TAY%AT5?APVAM
=AIC�AHjAEdZAC`BA@��A?x�A>z�A>bA=�PA;VA9�TA97LA8 �A7?}A5;dA3oA0I�A/oA.�jA-��A,��A+�mA*{A)�A)�A*~�A*{A)��A)?}A(r�A'��A&�A%�hA#�hA"A ffA�wA!oAXAQ�AM�A��A��A{Ap�AM�A1A%A�HA;dA�yAr�A�TA�;A��A�AAS�A;dA	��A	A�#A�9AA��A�A�Av�AƨAoA��A�TA|�AC�AO�A;dA%A �HA v�@���@��@�/@���@���@��;@�"�@�=q@���@��@�7@�@�`B@�p�@��@��@�Ĝ@�hs@�;d@�  @�/@Ұ!@ҧ�@�1@���@ҧ�@���@���@��@�^5@ɺ^@�&�@ƸR@�V@���@š�@š�@�x�@��@¸R@��
@š�@ļj@�~�@�M�@��#@�O�@�@��@�G�@��/@�x�@���@��\@�&�@�hs@�V@�Q�@���@���@��@��@�dZ@��!@�-@��@��@��@�K�@�K�@���@���@��@��#@���@�hs@��@��j@�j@��@��R@�@���@�o@��#@�$�@��T@��T@�E�@��T@��h@��@�bN@�l�@��@�I�@��9@�Z@�z�@�j@��@���@�r�@�33@�9X@��w@�A�@�z�@��D@��@��/@��7@��@��\@�M�@�p�@��@��j@�9X@�9X@�t�@�p�@�O�@�@��@�O�@�Ĝ@���@��@��@���@�
=@�V@� �@�=q@���@��h@�?}@�7L@�Ĝ@���@�V@���@��@��/@���@��j@���@��D@�Z@��@�j@�1'@�ƨ@��@��!@�|�@��F@�o@��\@�ff@��@�V@���@��m@�33@��@���@�E�@�M�@�ff@��@��h@�x�@��@��@��@�t�@�"�@�"�@�+@��@�=q@�5?@�V@��\@�{@��^@���@�x�@�`B@�/@��@�%@��@��/@���@��u@��@���@��u@�bN@�1'@��m@���@���@�|�@�\)@�+@��@���@��!@�v�@�V@�M�@��@���@�X@�X@�/@��@���@��H@���@�M�@�-@���@�x�@�O�@���@���@���@��@��@� �@�1'@�Ĝ@��@�^5@�S�@�n�@���@�ff@��@�@��^@�`B@��@���@��@�Q�@�b@�\)@���@�$�@��@���@��@�p�@��@��@� �@��P@���@�33@�"�@���@��!@��\@�n�@�M�@�J@���@���@���@��7@��7@�x�@�G�@�"�@|9X@s"�@cdZ@\�j@V5?@N�@G+@?;d@6�R@0�@,�D@'K�@"^5@p�@X@O�@��@
~�@K�@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A���A��A��A��A��yA��A��TA���A���A���A���A���A���A���A���A���A�ƨA�A���A���A���A���A���A���A�A�A�A�ȴA���A���A���A��#A���A���A�ĜA�M�A�S�A�VA��yAȍPA��#A�~�Aś�A�5?A�{A�`BA�l�A�z�A���A�S�A���A�ZA���A�A�v�A��-A���A���A���A�1A��A��;A��A� �A��yA���A��A��A��A�A�l�A�^5A��A��A�\)A��jA�1'A�ffA��jA�A�/A��A���A���A��A���A�JA�x�A�(�A��A���A�oA���A�r�A�$�A�Q�A��A��A�=qA�{A�
A{oAy�Ax��Aw33At��At��At�9At��AsƨAp��Al�HAj�Af(�Aa%A]��A[�mAZ{AY�TAY%AT5?APVAM
=AIC�AHjAEdZAC`BA@��A?x�A>z�A>bA=�PA;VA9�TA97LA8 �A7?}A5;dA3oA0I�A/oA.�jA-��A,��A+�mA*{A)�A)�A*~�A*{A)��A)?}A(r�A'��A&�A%�hA#�hA"A ffA�wA!oAXAQ�AM�A��A��A{Ap�AM�A1A%A�HA;dA�yAr�A�TA�;A��A�AAS�A;dA	��A	A�#A�9AA��A�A�Av�AƨAoA��A�TA|�AC�AO�A;dA%A �HA v�@���@��@�/@���@���@��;@�"�@�=q@���@��@�7@�@�`B@�p�@��@��@�Ĝ@�hs@�;d@�  @�/@Ұ!@ҧ�@�1@���@ҧ�@���@���@��@�^5@ɺ^@�&�@ƸR@�V@���@š�@š�@�x�@��@¸R@��
@š�@ļj@�~�@�M�@��#@�O�@�@��@�G�@��/@�x�@���@��\@�&�@�hs@�V@�Q�@���@���@��@��@�dZ@��!@�-@��@��@��@�K�@�K�@���@���@��@��#@���@�hs@��@��j@�j@��@��R@�@���@�o@��#@�$�@��T@��T@�E�@��T@��h@��@�bN@�l�@��@�I�@��9@�Z@�z�@�j@��@���@�r�@�33@�9X@��w@�A�@�z�@��D@��@��/@��7@��@��\@�M�@�p�@��@��j@�9X@�9X@�t�@�p�@�O�@�@��@�O�@�Ĝ@���@��@��@���@�
=@�V@� �@�=q@���@��h@�?}@�7L@�Ĝ@���@�V@���@��@��/@���@��j@���@��D@�Z@��@�j@�1'@�ƨ@��@��!@�|�@��F@�o@��\@�ff@��@�V@���@��m@�33@��@���@�E�@�M�@�ff@��@��h@�x�@��@��@��@�t�@�"�@�"�@�+@��@�=q@�5?@�V@��\@�{@��^@���@�x�@�`B@�/@��@�%@��@��/@���@��u@��@���@��u@�bN@�1'@��m@���@���@�|�@�\)@�+@��@���@��!@�v�@�V@�M�@��@���@�X@�X@�/@��@���@��H@���@�M�@�-@���@�x�@�O�@���@���@���@��@��@� �@�1'@�Ĝ@��@�^5@�S�@�n�@���@�ff@��@�@��^@�`B@��@���@��@�Q�@�b@�\)@���@�$�@��@���@��@�p�@��@��@� �@��P@���@�33@�"�@���@��!@��\@�n�@�M�@�J@���@���@���@��7@��7@�x�G�O�@�"�@|9X@s"�@cdZ@\�j@V5?@N�@G+@?;d@6�R@0�@,�D@'K�@"^5@p�@X@O�@��@
~�@K�@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�#B
�B
�B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�#B
�/B
�;B
�BB
�BB
�BB
�BB
�BB
�BB
�BB
�HB
�HB
�NB
�NB
�TB
�TB
�ZB
�`B
�fB
�mB
�mB
�sB
�B
�B
�B
�BB"�B/B]/B��B�B��B�B33B>wBW
B^5BZBZBk�Bw�B�B�bB��B�9B�FB�wBÖBǮBB�}BɺB�#B�B�B��B��B��B  B��B��B�/B�jB�B��B��B��B|�BR�B,B�;B��B�+B��B��Bw�Bl�B_;B{B
ŢB
�RB
�B
��B
�B
hsB
Q�B
I�B
8RB
0!B
E�B
7LB
"�B
�B
�B
�B
B	��B	�B	�ZB	��B	��B	��B	��B	ƨB	�'B	��B	�B	cTB	B�B	,B	 �B	�B	hB	1B�B�;B��B��BĜB�dB�jB�3B�^B�RB�LB�?B�RB�XB�RB�RB�?B�'B�B��B�B�3B��BÖBB�XB�dBɺB�NB�TB�TB�TB�HB�;B�fB�TB�BȴB�^B�LB��BǮB�3B�^B�/B�B�)B�FB�3B�}B��BƨB��B��B��BÖB�dB�RB�B�BȴB�jB�9B�?B�'B�B�-B�}B�dBÖBƨBŢB��B�}B�jB��BƨB��B�B�B�B�BɺBĜB��B��B��B��B��B��B��BŢBɺBɺB��B�#BƨB�3B��B�VB��B�hBz�Br�By�B�=B�{B�JB�1B� B}�B}�B� B� B�B�\B�uB�oB�{B��B��B��B��B�'B�^B�qBǮBǮBƨB��B��BĜBĜBɺB��B�5B�B��B	  B	B	bB	�B	�B	�B	
=B	DB	JB	DB	oB	oB	\B	(�B	7LB	R�B	_;B	`BB	cTB	dZB	dZB	dZB	ffB	k�B	jB	gmB	bNB	_;B	]/B	aHB	cTB	hsB	m�B	o�B	r�B	s�B	r�B	v�B	~�B	�B	�+B	�7B	�PB	�PB	�=B	�+B	�%B	�B	z�B	y�B	� B	�B	�B	�B	�1B	�PB	�bB	�{B	��B	�{B	�{B	��B	�{B	��B	�oB	�=B	�=B	�\B	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	�{B	�hB	�uB	��B	��B	��B	��B	�oB	�oB	�hB	�bB	�bB	�hB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�3B	�-B	�'B	�!B	�!B	�'B	�-B	�9B	�?B	�XB	�dB	��B	B	B	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�)B	�#B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	��B	��B	�
B	�#B	�NB	�`B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
\B
�B
 �B
.B
33B
8RB
>wB
F�B
M�B
VB
XB
]/B
aHB
dZB
jB
l�B
u�B
u�B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�%B
�B
�B
�#B
�#B
�'B
�#B
�#B
�#B
�)B
�%B
�2B
�?B
�FB
�FB
�CB
�HB
�HB
�FB
�FB
�LB
�LB
�QB
�OB
�TB
�UB
�[B
�aB
�hB
�pB
�pB
�qB
�B
�B
�B
�BB"�B/B].B��B�B��B�B31B>wBW	B^4BZBZBk�Bw�B�B�aB��B�;B�GB�zBÕBǭBB�|BɹB�%B�B�B��B��B��B B��B��B�1B�jB�	B��B��B��B|�BR�B,B�<B��B�'B��B��Bw�Bl�B_;BzB
ţB
�SB
�B
��B
�B
hxB
Q�B
I�B
8VB
0&B
E�B
7QB
"�B
�B
�B
�B
B	��B	�B	�bB	��B	��B	��B	��B	ưB	�2B	��B	�*B	c`B	B�B	,B	 �B	�B	vB	BB�B�MB�
B��BĮB�xB�}B�GB�qB�eB�_B�SB�dB�iB�dB�dB�RB�9B�&B�B�B�GB��BêB¡B�jB�wB��B�_B�eB�dB�dB�YB�MB�vB�dB�!B��B�qB�`B�BǿB�FB�qB�?B�B�9B�YB�EB��B��BƷB��B�B��BçB�vB�eB�"B�$B��B�zB�JB�QB�9B�B�AB��B�wBéBƸBŴB��B��B�{B��BƹB�B�'B�.B�(B�B��BıB��B��B��B��B��B��B��BųB��B��B�B�2BƷB�GB��B�iB��B�zBz�Br�By�B�SB��B�\B�CB�B~B~B�B�B�B�qB��B��B��B��B��B��B��B�9B�pB��BǽBǽBƸB��B��BĮBĭB��B��B�FB�B��B	 B	(B	qB	�B	�B	�B	
HB	QB	YB	PB	B	}B	hB	) B	7WB	R�B	_FB	`MB	c^B	ddB	dfB	ddB	fqB	k�B	j�B	guB	bVB	_FB	];B	aSB	c^B	h�B	m�B	o�B	r�B	s�B	r�B	v�B	B	�B	�2B	�=B	�\B	�YB	�DB	�5B	�-B	�B	z�B	y�B	�B	�B	�B	�!B	�:B	�ZB	�nB	��B	��B	��B	��B	��B	��B	��B	�uB	�GB	�FB	�fB	�`B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	�pB	�}B	��B	��B	��B	��B	�uB	�wB	�pB	�nB	�mB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�!B	�B	�B	�B	�B	�B	�"B	�(B	�/B	�<B	�?B	�;B	�4B	�.B	�)B	�(B	�-B	�2B	�@B	�FB	�_B	�jB	��B	B	B	ũB	ƮB	ǶB	ȼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�!B	�"B	�*B	� B	�-B	�)B	�B	�B	�B	�	B	�B	�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�RB	�dB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
G�O�B
B
aB
�B
 �B
.B
3:B
8RB
>yB
F�B
M�B
V	B
XB
]1B
aIB
d^B
j�B
l�B
u�B
u�B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608091433542016080914335420160809143354  AO  ARCAADJP                                                                    20160413134613    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160413134613  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160413134613  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160809143354  IP                  G�O�G�O�G�O�                