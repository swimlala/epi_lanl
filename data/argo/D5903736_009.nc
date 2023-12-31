CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:10Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230510  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               	A   AO  4051_7090_009                   2C  D   APEX                            5368                            041511                          846 @�H�$h�1   @�H��M��@3KƧ�d�7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    	A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�  D�P D���D���D��D�@ D��fD���D��3D�,�D�� Dǳ3D� D�S3D�s3D�ٚD� D�<�D�vfD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RC )C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�qDy��D� RD�PRD���D��D�D�@RD���D��D��D�-D��RDǳ�D�RD�S�D�s�D���D�RD�=D�v�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�ZA�/A�A���AθRAΝ�A·+A΃A�t�A�S�A�/A�1A���A��mA��HA��A��
A���A��#A��A�\)A�A�M�A���Aΰ!A��A�oA�  Aʡ�A�dZAȶFA�ffA�7LA���AǃA��A��`A�ĜAƗ�A�x�AŸRA�XA�A��AÛ�AA��A���A���A���A��A�A�33A�E�A�r�A��A�r�A��A���A�ffA�  A���A���A�ZA�ĜA��TA�O�A���A��HA� �A���A�S�A�|�A���A�;dA�M�A�l�A�A�E�A�n�A�G�A���A�/A��FA�z�A��FA��FA��A��A�1A�VA���A�ffA�VA�+A��9A��A�t�A�K�A�l�A��FA���A��PA��
A�
=A���A��PA�7LA���A�n�A��RA�l�A�K�A��FA�/A��9A���At�A~��A|��Az1AwVAt�uAqAm/Ak"�AjbNAjA�Aj5?Ai;dAdĜA`Q�A]l�A\�A[�TAZZAY��AYp�AXAVM�AU/AR�!AP=qAO�ANE�AMS�AL�yAL�DAKAJz�AI�AH=qAF��AD�AD��AD^5AC�FAB��AB �ABbAAt�A@^5A?p�A>�A=�-A<-A;K�A:1A9?}A8bNA6�/A5��A2��A1%A/�FA/�A.��A-t�A,�uA+A(r�A&(�A%��A$�9A"��A!�A!�A -A��A��AQ�A�
AQ�At�A7LAz�AhsA��A��A9XA�Ap�A��A�HA�#A
��A	&�A�7A�-AVA��A�+A��A|�A��A��A �A =q@���@�V@��T@��@�{@�G�@��D@�bN@�1@��P@��@�$�@���@�1'@�dZ@�{@�?}@�|�@�~�@���@�1@�S�@陚@���@��@��@�@�+@�{@噚@�9X@�~�@ᙚ@��@�  @��y@ݡ�@ܼj@�@�~�@ٺ^@�`B@׶F@ӥ�@��H@��`@�b@�5?@���@�O�@ˮ@˅@�dZ@�+@��@�Z@��@�J@�7L@�%@���@�j@��
@�
=@�&�@��@�Q�@��@���@���@�~�@��@�O�@�Ĝ@��9@��u@�j@��w@���@�J@��`@�1'@�K�@���@�~�@�=q@��@���@��D@�ƨ@��@���@�^5@�J@���@�O�@��@���@���@�-@�J@���@�`B@�r�@���@���@�dZ@��H@�M�@���@��-@�x�@�O�@��@��9@�j@�Q�@�A�@� �@��m@���@�S�@��@���@��\@���@�?}@��@���@���@�Q�@��F@���@�l�@�;d@��@���@��H@�~�@�@��#@���@�`B@�O�@�V@��@��j@�1@�dZ@�;d@��@�
=@�
=@�@���@�v�@�ff@�$�@��@�@�hs@�/@��@���@��@�Z@� �@�1@�1@��m@�ƨ@���@��P@�t�@�C�@��@��!@��\@�n�@�5?@���@�?}@���@���@���@��@�  @��
@��@�dZ@��@��!@��\@�ff@�{@��T@���@�hs@���@���@��u@�bN@�9X@�b@���@�dZ@�@��y@���@��+@��@�x�@�/@���@���@���@�j@�(�@��@���@��@��@�\)@�C�@�+@��@���@���@���@�M�@�-@��@���@��-@���@���@���@��h@��7@��h@�p�@��`@���@�bN@�Z@�(�@�1'@�1'@��m@��@�l�@�K�@��@���@��\@�ff@�5?@��@���@��7@��@�X@�?}@�7L@�&�@��@�V@���@���@��9@�
=@�v�@{�@q��@g��@]�@V{@O|�@I%@C��@>5?@7|�@1�@)�#@$��@;d@9X@�@��@�@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ZA�ZA�/A�A���AθRAΝ�A·+A΃A�t�A�S�A�/A�1A���A��mA��HA��A��
A���A��#A��A�\)A�A�M�A���Aΰ!A��A�oA�  Aʡ�A�dZAȶFA�ffA�7LA���AǃA��A��`A�ĜAƗ�A�x�AŸRA�XA�A��AÛ�AA��A���A���A���A��A�A�33A�E�A�r�A��A�r�A��A���A�ffA�  A���A���A�ZA�ĜA��TA�O�A���A��HA� �A���A�S�A�|�A���A�;dA�M�A�l�A�A�E�A�n�A�G�A���A�/A��FA�z�A��FA��FA��A��A�1A�VA���A�ffA�VA�+A��9A��A�t�A�K�A�l�A��FA���A��PA��
A�
=A���A��PA�7LA���A�n�A��RA�l�A�K�A��FA�/A��9A���At�A~��A|��Az1AwVAt�uAqAm/Ak"�AjbNAjA�Aj5?Ai;dAdĜA`Q�A]l�A\�A[�TAZZAY��AYp�AXAVM�AU/AR�!AP=qAO�ANE�AMS�AL�yAL�DAKAJz�AI�AH=qAF��AD�AD��AD^5AC�FAB��AB �ABbAAt�A@^5A?p�A>�A=�-A<-A;K�A:1A9?}A8bNA6�/A5��A2��A1%A/�FA/�A.��A-t�A,�uA+A(r�A&(�A%��A$�9A"��A!�A!�A -A��A��AQ�A�
AQ�At�A7LAz�AhsA��A��A9XA�Ap�A��A�HA�#A
��A	&�A�7A�-AVA��A�+A��A|�A��A��A �A =q@���@�V@��T@��@�{@�G�@��D@�bN@�1@��P@��@�$�@���@�1'@�dZ@�{@�?}@�|�@�~�@���@�1@�S�@陚@���@��@��@�@�+@�{@噚@�9X@�~�@ᙚ@��@�  @��y@ݡ�@ܼj@�@�~�@ٺ^@�`B@׶F@ӥ�@��H@��`@�b@�5?@���@�O�@ˮ@˅@�dZ@�+@��@�Z@��@�J@�7L@�%@���@�j@��
@�
=@�&�@��@�Q�@��@���@���@�~�@��@�O�@�Ĝ@��9@��u@�j@��w@���@�J@��`@�1'@�K�@���@�~�@�=q@��@���@��D@�ƨ@��@���@�^5@�J@���@�O�@��@���@���@�-@�J@���@�`B@�r�@���@���@�dZ@��H@�M�@���@��-@�x�@�O�@��@��9@�j@�Q�@�A�@� �@��m@���@�S�@��@���@��\@���@�?}@��@���@���@�Q�@��F@���@�l�@�;d@��@���@��H@�~�@�@��#@���@�`B@�O�@�V@��@��j@�1@�dZ@�;d@��@�
=@�
=@�@���@�v�@�ff@�$�@��@�@�hs@�/@��@���@��@�Z@� �@�1@�1@��m@�ƨ@���@��P@�t�@�C�@��@��!@��\@�n�@�5?@���@�?}@���@���@���@��@�  @��
@��@�dZ@��@��!@��\@�ff@�{@��T@���@�hs@���@���@��u@�bN@�9X@�b@���@�dZ@�@��y@���@��+@��@�x�@�/@���@���@���@�j@�(�@��@���@��@��@�\)@�C�@�+@��@���@���@���@�M�@�-@��@���@��-@���@���@���@��h@��7@��h@�p�@��`@���@�bN@�Z@�(�@�1'@�1'@��m@��@�l�@�K�@��@���@��\@�ff@�5?@��@���@��7@��@�X@�?}@�7L@�&�@��@�V@���@���G�O�@�
=@�v�@{�@q��@g��@]�@V{@O|�@I%@C��@>5?@7|�@1�@)�#@$��@;d@9X@�@��@�@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�jB�BM�B�B�B�B�RBÖBȴB�XB�jB��B��B�B�TB�B%B�B�B�B$�B#�B#�B!�B'�B+B0!B/B'�B�B�B��B��B�uB�=B��B��B��B��B��B��B��B�?B�LB�LB�LB�FB�?B��BÖB�wB�3B�B��B��B��B��B�JB� By�Br�BhsBbNB[#BS�BE�B<jB33B'�B!�B�BB��B�fB��B��B�FB��B�\B|�Bn�B`BBC�B+B�BPBB
�B
�B
�^B
�oB
�B
`BB
N�B
F�B
?}B
49B
"�B
DB
B	��B	�`B	��B	�wB	��B	��B	�JB	�1B	�+B	�%B	}�B	e`B	L�B	E�B	H�B	I�B	M�B	R�B	YB	[#B	Q�B	J�B	?}B	49B	33B	1'B	1'B	0!B	,B	&�B	%�B	#�B	�B	�B	JB	
=B	
=B	JB	VB	PB	JB	
=B	%B	B��B��B�B�B�B�mB�NB�/B�B��B��B��B��BɺBƨBB�qB�LB�-B�B�B��B��B��B��B��B��B�{B�bB�PB�PB�+B�B�B~�B|�Bz�Bw�Bu�Bu�Bs�Bq�Bn�Bk�BhsBe`BdZBdZBdZBcTBbNBbNBbNBbNBbNBbNBbNBaHB`BBaHBbNBgmBjBiyBiyBiyBiyBiyBiyBiyBjBjBl�Bn�Bp�Bo�Bp�Br�Bv�By�B{�Bz�B� B� B�B�B~�B~�B�B�B�%B�B�%B�=B�PB�\B�hB�oB�hB�bB�hB�bB�uB�{B�{B��B��B��B��B��B��B��B�B�-B�3B�3B�9B�9B�?B�^B�jBBĜBŢBǮBɺB��B��B��B��B��B��B��B�B�#B�)B�;B�fB�yB�B�B�B�B��B��B��B	  B	B	+B		7B	JB	bB	�B	�B	 �B	!�B	#�B	&�B	-B	0!B	1'B	33B	6FB	;dB	?}B	@�B	B�B	C�B	D�B	H�B	K�B	K�B	M�B	M�B	N�B	Q�B	S�B	S�B	T�B	YB	^5B	bNB	dZB	e`B	gmB	jB	l�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	t�B	u�B	{�B	~�B	~�B	~�B	�B	�B	�B	�7B	�=B	�DB	�JB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�qB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
JB
PB
uB
�B
#�B
+B
33B
9XB
>wB
C�B
G�B
L�B
R�B
W
B
_;B
cTB
hsB
k�B
q�B
t�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�hB�BM�B�B�B�B�VBÔBȱB�[B�mB��B��B�B�SB�B%B�B�B�B$�B#�B#�B!�B'�B+B0#B/B'�B�B�B��B��B�vB�<B�B��B��B��B��B��B��B�@B�MB�LB�JB�GB�AB��BÖB�yB�6B�
B��B��B��B��B�JB�By�Br�BhtBbPB[ BS�BE�B<gB30B'�B!�B�BB��B�bB��B��B�FB��B�[B|�Bn�B`ABC�B+B�BOB"B
�B
�B
�`B
�qB
�B
`HB
N�B
F�B
?�B
4AB
"�B
LB
!B	��B	�gB	��B	��B	��B	��B	�UB	�;B	�4B	�0B	~ B	ekB	L�B	E�B	H�B	I�B	M�B	R�B	Y$B	[,B	Q�B	J�B	?�B	4FB	3@B	14B	13B	0-B	,B	&�B	%�B	#�B	�B	�B	ZB	
KB	
LB	YB	eB	aB	WB	
LB	6B	B��B��B�B�B�B�B�_B�AB�(B��B��B��B��B��BƹB£B��B�^B�@B�0B�B��B��B��B��B��B��B��B�uB�bB�cB�?B�0B�BB}Bz�Bw�Bu�Bu�Bs�Bq�Bn�Bk�Bh�BevBdmBdnBdoBchBbdBbcBbdBbcBbcBbfBbcBa^B`XBa^BbcBg�Bj�Bi�Bi�Bi�Bi�Bi�Bi�Bi�Bj�Bj�Bl�Bn�Bp�Bo�Bp�Br�Bv�By�B{�Bz�B�B�B�B�BBB�B�3B�8B�2B�9B�RB�dB�pB�|B��B�|B�vB�zB�uB��B��B��B��B��B��B��B��B��B�B�+B�=B�CB�DB�JB�KB�QB�nB�zBBĭBűBǽB��B��B��B��B��B��B��B�B�B�1B�8B�JB�uB�B�B�B�B�B��B��B��B	 B	%B	7B		EB	WB	oB	�B	�B	 �B	!�B	#�B	&�B	-B	0,B	12B	3>B	6PB	;pB	?�B	@�B	B�B	C�B	D�B	H�B	K�B	K�B	M�B	M�B	N�B	Q�B	TB	TB	UB	Y B	^?B	bWB	deB	ehB	gwB	j�B	l�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	t�B	u�B	{�B	B	B	B	�B	�B	�&B	�>B	�FB	�MB	�TB	�\B	�dB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�#B	�<B	�LB	�TB	�RB	�XB	�`B	�fB	�jB	�yB	�~B	��B	ÜB	ģB	ŪB	ƮB	ǷB	ǴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�#B	�.B	�4B	�=B	�BB	�IB	�IB	�MB	�SB	�ZB	�ZB	�`B	�eB	�lB	�yB	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B
 B
B
B
B
B
!B
B
"B
,B
+B
+B
(B
+B
2B
7G�O�B
UB
{B
�B
#�B
+	B
37B
9[B
>{B
C�B
G�B
L�B
R�B
WB
_>B
cWB
hvB
k�B
q�B
t�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230510    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230510  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230510  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                