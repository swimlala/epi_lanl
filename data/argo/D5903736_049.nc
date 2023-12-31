CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:32Z AOML 3.0 creation; 2016-05-31T19:14:32Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230532  20160531121432  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               1A   AO  4051_7090_049                   2C  D   APEX                            5368                            041511                          846 @֯eC"1   @֯ �/�@4�$�/��d�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    1A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D�fD�I�D�vfD���D�fD�6fD��fD�ɚD�  D�I�D�� D�� D�fD�L�D�i�D���D�fD�6fD�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A��HA�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RC )C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP)CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI
DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dts�Dyz>D��D�I�D�v�D���D��D�6�D���D���D� RD�I�D��RD��RD��D�MD�i�D��D��D�6�D�pRD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AܾwAܕ�A܇+A�;dA�(�A�{A��A��;A��A��
A���A���A���A�A۾wAۺ^Aۧ�A�\)A��;Aӝ�A�O�AΕ�A���A�O�A�%AǛ�A�VAƕ�AżjAÕ�A�jA��A���A�ZA��A���A�C�A���A�$�A��+A���A�t�A�I�A�1'A�+A���A��A�VA��HA��A�JA�7LA���A�E�A���A�K�A��yA���A�~�A�?}A�M�A��A�7LA�G�A�t�A��A�ȴA�-A��A��A�l�A���A��#A�7LA�A�A��A���A�/A�oA�n�A���A��A���A���A���A���A�~�A�/A�/A��PA�$�A��A��FA���A� �A��A��;A�33A�;dA���A�|�A�#AO�A~��A|�\Az�uAs�Aqx�Ap�!Ao`BAm��Aj�Ah�`Ah5?Ag�FAf �Act�AadZA`�!A_�^A^�A^��A]�PA[�hAZQ�AY��AY?}AY/AX��AV��AU7LAS�PAR �AP�jAPZAN�\AMt�AM"�ALI�AKoAHZAF��AE��ADZACt�AB�/AB��AB9XAB{AA"�A?��A>�+A=�A<�HA;��A:  A7�FA5?}A3�A1G�A/�A.~�A-�A+�A+%A*��A*I�A)��A(��A'�#A&��A%��A#A#C�A"�yA!��A M�Ax�A�uA�At�A��AbA�mA��A"�A�uA`BA��A�-AAt�Ap�AQ�A�
AG�AĜA;dA^5A
��A	\)A5?A��AS�A��AbNAt�A��A~�A�A(�A�-A%A ~�A  �@�n�@�/@�J@�;d@��7@��u@�\)@�J@�%@��m@��@�+@��@�z�@�S�@��y@��@��@�@��@��@��@߶F@�@��@��@��#@�@ՙ�@�x�@�O�@�C�@�=q@���@�Ĝ@ϝ�@��@�n�@�E�@�@�7L@���@�9X@�ƨ@˅@�33@�v�@�x�@�(�@�"�@�ȴ@�V@�@���@�b@��;@�"�@�~�@�@���@�z�@�;d@�7L@��@�Z@�S�@�J@���@��@��#@��h@�/@�9X@��w@���@�+@�@���@��@���@��;@�dZ@�+@���@�v�@��^@���@��9@�  @�S�@�@�^5@���@�`B@��/@�Q�@�b@��F@�C�@�o@��@���@�E�@���@���@��/@���@�I�@�1@��
@�\)@��H@��+@�J@��-@���@�?}@��`@�z�@�1'@��;@��F@��@�dZ@�
=@��@��y@�v�@�-@��@��#@���@��^@���@�x�@�O�@�V@���@��j@��u@�r�@�(�@���@��;@��w@�t�@�C�@��H@���@�$�@���@��@��^@�G�@���@��`@���@��u@�I�@�|�@��@�@��@���@���@���@�V@�{@���@��7@�O�@�?}@�7L@�7L@�7L@��@���@��/@���@��u@�bN@� �@��@��m@��w@�ƨ@���@��F@��@��@�|�@�S�@�o@�o@���@���@��@��@��@�@���@�E�@�J@��#@���@���@��7@�/@���@���@���@���@���@��j@���@�z�@�bN@�Q�@�A�@�1'@��@��;@���@�S�@�+@�"�@��!@��@��^@�x�@�?}@�V@���@��9@��9@���@�r�@��@��F@�dZ@��@��H@���@�v�@�{@���@���@��@�&�@���@���@��9@���@��D@�j@�A�@�(�@�  @�ƨ@�t�@�33@�@��H@�ȴ@���@�~�@�{@��#@�`B@�&�@�V@�V@���@�5?@x �@o�P@h1'@b��@ZM�@RM�@Mp�@G\)@A7L@8�`@0r�@+"�@%�h@ �`@J@|�@Z@l�@�-@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AܾwAܕ�A܇+A�;dA�(�A�{A��A��;A��A��
A���A���A���A�A۾wAۺ^Aۧ�A�\)A��;Aӝ�A�O�AΕ�A���A�O�A�%AǛ�A�VAƕ�AżjAÕ�A�jA��A���A�ZA��A���A�C�A���A�$�A��+A���A�t�A�I�A�1'A�+A���A��A�VA��HA��A�JA�7LA���A�E�A���A�K�A��yA���A�~�A�?}A�M�A��A�7LA�G�A�t�A��A�ȴA�-A��A��A�l�A���A��#A�7LA�A�A��A���A�/A�oA�n�A���A��A���A���A���A���A�~�A�/A�/A��PA�$�A��A��FA���A� �A��A��;A�33A�;dA���A�|�A�#AO�A~��A|�\Az�uAs�Aqx�Ap�!Ao`BAm��Aj�Ah�`Ah5?Ag�FAf �Act�AadZA`�!A_�^A^�A^��A]�PA[�hAZQ�AY��AY?}AY/AX��AV��AU7LAS�PAR �AP�jAPZAN�\AMt�AM"�ALI�AKoAHZAF��AE��ADZACt�AB�/AB��AB9XAB{AA"�A?��A>�+A=�A<�HA;��A:  A7�FA5?}A3�A1G�A/�A.~�A-�A+�A+%A*��A*I�A)��A(��A'�#A&��A%��A#A#C�A"�yA!��A M�Ax�A�uA�At�A��AbA�mA��A"�A�uA`BA��A�-AAt�Ap�AQ�A�
AG�AĜA;dA^5A
��A	\)A5?A��AS�A��AbNAt�A��A~�A�A(�A�-A%A ~�A  �@�n�@�/@�J@�;d@��7@��u@�\)@�J@�%@��m@��@�+@��@�z�@�S�@��y@��@��@�@��@��@��@߶F@�@��@��@��#@�@ՙ�@�x�@�O�@�C�@�=q@���@�Ĝ@ϝ�@��@�n�@�E�@�@�7L@���@�9X@�ƨ@˅@�33@�v�@�x�@�(�@�"�@�ȴ@�V@�@���@�b@��;@�"�@�~�@�@���@�z�@�;d@�7L@��@�Z@�S�@�J@���@��@��#@��h@�/@�9X@��w@���@�+@�@���@��@���@��;@�dZ@�+@���@�v�@��^@���@��9@�  @�S�@�@�^5@���@�`B@��/@�Q�@�b@��F@�C�@�o@��@���@�E�@���@���@��/@���@�I�@�1@��
@�\)@��H@��+@�J@��-@���@�?}@��`@�z�@�1'@��;@��F@��@�dZ@�
=@��@��y@�v�@�-@��@��#@���@��^@���@�x�@�O�@�V@���@��j@��u@�r�@�(�@���@��;@��w@�t�@�C�@��H@���@�$�@���@��@��^@�G�@���@��`@���@��u@�I�@�|�@��@�@��@���@���@���@�V@�{@���@��7@�O�@�?}@�7L@�7L@�7L@��@���@��/@���@��u@�bN@� �@��@��m@��w@�ƨ@���@��F@��@��@�|�@�S�@�o@�o@���@���@��@��@��@�@���@�E�@�J@��#@���@���@��7@�/@���@���@���@���@���@��j@���@�z�@�bN@�Q�@�A�@�1'@��@��;@���@�S�@�+@�"�@��!@��@��^@�x�@�?}@�V@���@��9@��9@���@�r�@��@��F@�dZ@��@��H@���@�v�@�{@���@���@��@�&�@���@���@��9@���@��D@�j@�A�@�(�@�  @�ƨ@�t�@�33@�@��H@�ȴ@���@�~�@�{@��#@�`B@�&�@�V@�V@���@�5?@x �@o�P@h1'@b��@ZM�@RM�@Mp�@G\)@A7L@8�`@0r�@+"�@%�h@ �`@J@|�@Z@l�@�-@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�wB��B�qB�RB�LB�?B�-B�B��B��B��B��B��B��B��B�bB�DB�+B�B�B}�B{�By�Bw�Bl�Be`BhsBbNBD�B9XB0!B'�BuB1BB  B��B��B�B�B�fB�#B��B�LB�B�B��B�bB|�Bl�B`BBR�B:^B)�B�BB�B��BƨB�jB�-B��B��B�VB�=B�Bn�BN�B5?B�BPB
��B
�
B
�3B
��B
�\B
w�B
iyB
`BB
P�B
H�B
D�B
>wB
/B
�B	��B	�B	�mB	�BB	��B	ŢB	�jB	�RB	�9B	�B	��B	�{B	�bB	�JB	�1B	�%B	� B	w�B	q�B	o�B	l�B	k�B	gmB	_;B	W
B	O�B	H�B	B�B	@�B	9XB	49B	2-B	.B	&�B	�B	�B	uB	VB	DB	1B	+B	%B	B	B��B��B�B�B�B�TB�B��B��B�jB�RB�3B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�VB�JB�DB�=B�7B�+B�+B�+B�B�B�B~�B|�By�Bv�Bs�Br�Bp�Bm�BjBhsBffBe`BdZBcTBcTBbNBaHBaHB`BB_;B^5B^5B^5B^5B^5B^5B]/B^5B\)B]/B^5B_;B^5B^5B]/B]/B\)B[#BZBYB\)B]/B]/B]/B]/B]/B\)B`BBcTBcTBcTBcTBe`BffBffBffBffBe`Bk�Bq�Bs�Bv�B{�B~�B� B�B�B�B�B�B�B�B�B�B�B�7B�PB�\B�bB�bB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�B�'B�9B�^B�wB�wB��B��BÖBȴB��B��B��B��B��B�B�B�5B�;B�HB�ZB�`B�mB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	%B		7B	DB	JB	\B	hB	uB	�B	�B	�B	�B	�B	"�B	$�B	)�B	.B	0!B	1'B	49B	6FB	7LB	<jB	@�B	C�B	D�B	E�B	F�B	G�B	I�B	K�B	N�B	R�B	S�B	VB	W
B	[#B	^5B	`BB	cTB	e`B	hsB	jB	l�B	p�B	q�B	r�B	s�B	w�B	{�B	{�B	|�B	� B	�B	�7B	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�RB	�XB	�dB	�qB	�}B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�ZB	�`B	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
�B
�B
#�B
,B
1'B
8RB
>wB
C�B
H�B
N�B
XB
\)B
aHB
e`B
k�B
m�B
o�B
s�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�
B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�1B�LB��B��B�xB�WB�SB�GB�1B�B��B��B��B��B��B��B��B�gB�JB�.B�B�B}�B{�By�Bw�Bl�BedBhvBbPBD�B9]B0&B'�BvB5BB��B��B��B�B�B�fB�$B��B�KB�B�B��B�dB|�Bl�B`DBR�B:\B)�B�BB�B��BƧB�iB�0B��B��B�VB�>B�Bn�BN�B5>B�BQB
��B
�B
�6B
��B
�bB
w�B
i~B
`CB
P�B
H�B
D�B
>|B
/$B
�B	��B	�B	�uB	�KB	�B	ūB	�sB	�\B	�AB	�B	��B	��B	�oB	�SB	�=B	�.B	�
B	w�B	q�B	o�B	l�B	k�B	gyB	_GB	WB	O�B	H�B	B�B	@�B	9cB	4DB	2:B	.#B	&�B	�B	�B	�B	fB	SB	AB	:B	3B	-B	B��B��B��B�B�B�eB�"B��B��B�}B�dB�EB�.B�B�B�	B�B��B��B��B��B��B��B��B��B��B�wB�hB�^B�XB�RB�LB�?B�AB�BB�3B�&B�BB}By�Bv�Bs�Br�Bp�Bm�Bj�Bh�BfBewBdnBckBciBbcBa_Ba_B`YB_OB^KB^KB^KB^IB^MB^LB]EB^KB\=B]BB^MB_RB^IB^JB]DB]DB\>B[:BZ4BY-B\<B]EB]CB]EB]DB]DB\>B`XBciBcjBciBciBetBf|Bf{Bf{BfzBeuBk�Bq�Bs�Bv�B{�BB�B�B�B�&B�&B�'B�'B�B�!B�&B�-B�HB�cB�pB�uB�sB��B��B��B��B��B��B��B��B��B��B�	B�'B� B�B�B�B�,B�9B�KB�qB��B��B��B��BæB��B��B��B�B�B�B�B�0B�DB�IB�XB�jB�nB�{B�B�B�B�B�B�B��B��B��B��B��B�B	 B	'B	3B		EB	QB	ZB	gB	tB	�B	�B	�B	�B	�B	�B	"�B	$�B	*	B	.!B	0-B	11B	4DB	6QB	7VB	<tB	@�B	C�B	D�B	E�B	F�B	G�B	I�B	K�B	N�B	R�B	TB	VB	WB	[.B	^=B	`JB	c\B	ejB	hB	j�B	l�B	p�B	q�B	r�B	s�B	w�B	{�B	{�B	|�B	�B	�B	�AB	�LB	�]B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�!B	�'B	�4B	�9B	�FB	�NB	�ZB	�_B	�mB	�xB	��B	B	ÛB	ĢB	ūB	ǵB	ȸB	��B	��B	��B	��B	��B	�B	�B	�#B	�+B	�)B	�+B	�/B	�5B	�5B	�5B	�2B	�=B	�@B	�HB	�GB	�OB	�OB	�OB	�PB	�MB	�SB	�_B	�fB	�dB	�fB	�sB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
B
�B
�B
#�B
,B
1+B
8VB
>xB
C�B
H�B
N�B
XB
\*B
aHB
eeB
k�B
m�B
o�B
s�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214322016053112143220160531121432  AO  ARCAADJP                                                                    20140721230532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230532  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121432  IP                  G�O�G�O�G�O�                