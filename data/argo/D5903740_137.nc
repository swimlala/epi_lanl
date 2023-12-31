CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-29T03:16:14Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151229031614  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_137                   2C  D   APEX                            5374                            041511                          846 @׉�c]�L1   @׉��`r�@:[dZ��c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyY�D��D�L�D�vfD���D�3D�L�D�|�D��fD���D�L�D�� D�� D�	�D�I�Dڣ3D��D�3D�,�D�s3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�z�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�BhBq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{B�ǮB�aHB�aHBܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt\Dyl)D�&D�VD��D��D�{D�VD��D�߮D��D�VD��HD��HD��D�R�Dڬ{D��D�{D�6D�|{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A��uA��\A��\A��hA��hA��\A��DA��DA��DA��7A��7A��DA��DA��DA��DA��DA��PA��hA��hA��uA��hA��hA��uA���A��uA��\A��\A��hA��hA��hA��uA��uA���A��hA�~�A�z�A�|�A�bNA���A���A���A���A�^5A��7A�I�A�E�A�
=A��RA��uA���A�7LA�33A�K�A��A�%A�bNA�bNA���A�G�A���A���A��7A��A��^A�{A�ȴA��HA���A�t�A�~�A�O�A�O�A��hA�ȴA��RA���A�hsA�|�A|�A~r�A|��A{S�Az�`Az^5Ay
=Ax$�Aw��Aw�AwdZAvbNAu/As�ApffAmdZAk`BAhr�AgAfVAeXAdM�Acp�Ac�Ab��AbbAa`BA`��A_;dA[�TAY�;AX��AXZAW�AV�`AV^5AU��AT�AS�;AS|�AS+AR�`ARVAQ�-AP^5AO��ANM�AM/AJ��AG�-AE�PAD��ADr�AC�;AC�7AC\)AB�RABr�ABn�AB�AA|�A>��A=33A<9XA<{A;�7A:��A:��A:�A9�A8v�A7�A6��A5�TA4�A4JA3�TA3�wA3G�A2�A1�A1/A/�A.�uA-��A,z�A+A*��A)��A)?}A(��A'x�A&~�A&5?A%��A${A#�A"E�A!��A Q�A��A7LA9XA��A��A�7A�AhsAE�AS�A�`A�9AjA1A�-A?}A�AZA�/A1'A�wAl�A-AG�A;dA�A�A�wAG�A
�/A
Q�A	��A��A�mA�A�AĜA^5AA��Ap�Az�A�PA �\@���@��h@�b@�$�@�G�@��@��@�(�@�1'@��
@�\)@�J@�O�@�r�@�@�+@�?}@�@�x�@�z�@��
@�^5@�h@�7L@�r�@���@���@�A�@�1@�@��H@�J@�O�@ߍP@�x�@܃@�l�@�?}@�b@�\)@֟�@���@ӍP@�ȴ@�O�@�dZ@�J@���@���@��#@��`@�bN@��;@���@��T@Ł@Ĭ@��@�@��#@�7L@���@�;d@��@���@��\@��@�O�@���@���@��@�l�@�~�@��@��@�@�$�@�hs@���@�r�@�1@��@�M�@�J@��#@���@��@�t�@��H@���@�~�@�r�@��@��@���@���@�n�@�-@��@�@��h@�`B@��@�j@�S�@��R@�5?@��T@�x�@�X@�7L@��@��w@���@�@��h@�X@�G�@���@�r�@�(�@��F@�\)@��H@���@�~�@�@��h@���@�j@�(�@��;@�ƨ@��@��P@��y@�=q@�J@���@�x�@���@�  @��@�C�@�"�@��@�n�@�J@��@�-@�{@���@�?}@�%@��j@�Z@�Q�@�  @��
@���@�\)@�+@�o@�o@�ȴ@�{@��@���@�hs@�p�@���@��@��D@�t�@���@��\@�=q@���@��7@�`B@�7L@�V@��`@�Q�@��@�K�@��@�"�@��+@�E�@��#@��@�&�@��@�bN@�Q�@��m@�|�@�33@�o@���@��@��y@��H@�ȴ@���@��\@�~�@�=q@��@�@���@��-@�$�@�J@�?}@��`@���@�Ĝ@�7L@�p�@���@�A�@�1@~�y@~$�@}��@�;@�9X@�b@�b@�  @�z�@��@�A�@��@�b@�@�@�;@��@��@\)@
=@~��@}�-@}`B@|��@|I�@|9X@{33@z��@zJ@y�#@y�7@y&�@x��@w��@w+@t��@n5?@c�m@\j@V5?@O�P@Hb@A�#@:�H@4�@/�P@*^5@"�@{@  @��@\)@
�!@ �@p�@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A��uA��\A��\A��hA��hA��\A��DA��DA��DA��7A��7A��DA��DA��DA��DA��DA��PA��hA��hA��uA��hA��hA��uA���A��uA��\A��\A��hA��hA��hA��uA��uA���A��hA�~�A�z�A�|�A�bNA���A���A���A���A�^5A��7A�I�A�E�A�
=A��RA��uA���A�7LA�33A�K�A��A�%A�bNA�bNA���A�G�A���A���A��7A��A��^A�{A�ȴA��HA���A�t�A�~�A�O�A�O�A��hA�ȴA��RA���A�hsA�|�A|�A~r�A|��A{S�Az�`Az^5Ay
=Ax$�Aw��Aw�AwdZAvbNAu/As�ApffAmdZAk`BAhr�AgAfVAeXAdM�Acp�Ac�Ab��AbbAa`BA`��A_;dA[�TAY�;AX��AXZAW�AV�`AV^5AU��AT�AS�;AS|�AS+AR�`ARVAQ�-AP^5AO��ANM�AM/AJ��AG�-AE�PAD��ADr�AC�;AC�7AC\)AB�RABr�ABn�AB�AA|�A>��A=33A<9XA<{A;�7A:��A:��A:�A9�A8v�A7�A6��A5�TA4�A4JA3�TA3�wA3G�A2�A1�A1/A/�A.�uA-��A,z�A+A*��A)��A)?}A(��A'x�A&~�A&5?A%��A${A#�A"E�A!��A Q�A��A7LA9XA��A��A�7A�AhsAE�AS�A�`A�9AjA1A�-A?}A�AZA�/A1'A�wAl�A-AG�A;dA�A�A�wAG�A
�/A
Q�A	��A��A�mA�A�AĜA^5AA��Ap�Az�A�PA �\@���@��h@�b@�$�@�G�@��@��@�(�@�1'@��
@�\)@�J@�O�@�r�@�@�+@�?}@�@�x�@�z�@��
@�^5@�h@�7L@�r�@���@���@�A�@�1@�@��H@�J@�O�@ߍP@�x�@܃@�l�@�?}@�b@�\)@֟�@���@ӍP@�ȴ@�O�@�dZ@�J@���@���@��#@��`@�bN@��;@���@��T@Ł@Ĭ@��@�@��#@�7L@���@�;d@��@���@��\@��@�O�@���@���@��@�l�@�~�@��@��@�@�$�@�hs@���@�r�@�1@��@�M�@�J@��#@���@��@�t�@��H@���@�~�@�r�@��@��@���@���@�n�@�-@��@�@��h@�`B@��@�j@�S�@��R@�5?@��T@�x�@�X@�7L@��@��w@���@�@��h@�X@�G�@���@�r�@�(�@��F@�\)@��H@���@�~�@�@��h@���@�j@�(�@��;@�ƨ@��@��P@��y@�=q@�J@���@�x�@���@�  @��@�C�@�"�@��@�n�@�J@��@�-@�{@���@�?}@�%@��j@�Z@�Q�@�  @��
@���@�\)@�+@�o@�o@�ȴ@�{@��@���@�hs@�p�@���@��@��D@�t�@���@��\@�=q@���@��7@�`B@�7L@�V@��`@�Q�@��@�K�@��@�"�@��+@�E�@��#@��@�&�@��@�bN@�Q�@��m@�|�@�33@�o@���@��@��y@��H@�ȴ@���@��\@�~�@�=q@��@�@���@��-@�$�@�J@�?}@��`@���@�Ĝ@�7L@�p�@���@�A�@�1@~�y@~$�@}��@�;@�9X@�b@�b@�  @�z�@��@�A�@��@�b@�@�@�;@��@��@\)@
=@~��@}�-@}`B@|��@|I�@|9X@{33@z��@zJ@y�#@y�7@y&�@x��@w��@w+@t��@n5?@c�m@\j@V5?@O�P@Hb@A�#@:�H@4�@/�P@*^5@"�@{@  @��@\)@
�!@ �@p�@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B^5B_;B^5B]/BZBYB?}B$�BoBDBB��B�3BYBH�B2-B�B1B��B�B�BB��BȴB�jB�3B�B��B��B��B��B�\Bn�B\)BN�BA�B/B%�B�BPBB
��B
�`B
�B
��B
��B
�^B
�3B
��B
��B
��B
��B
�PB
�+B
�B
�B
}�B
q�B
e`B
S�B
7LB
�B
1B	�B	�ZB	�5B	�
B	��B	ǮB	ĜB	��B	�jB	�LB	�!B	��B	�uB	�1B	�B	� B	z�B	u�B	q�B	l�B	e`B	`BB	]/B	[#B	YB	T�B	N�B	E�B	?}B	5?B	,B	�B		7B	  B��B��B��B��B��B��B��B��B�B�B�B�sB�`B�`B�`B�NB�HB�BB�/B�)B�B�B�B��B��B��B��BɺBȴBȴBƨBB�}B�dB�LB�3B�!B�B�B��B��B��B��B��B��B��B�uB�bB�=B�B� B}�B|�B|�B{�Bx�Bt�Bq�Bo�Bn�Bn�Bm�Bk�BjBgmBbNB\)BYBXBXBVBS�BQ�BP�BP�BO�BM�BL�BJ�BH�BG�BF�BE�BD�BC�BB�BA�B?}B;dB9XB8RB7LB7LB7LB5?B49B1'B0!B0!B1'B2-B33B2-B2-B1'B1'B0!B/B.B,B+B+B)�B(�B(�B(�B'�B'�B&�B%�B%�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B(�B'�B,B.B.B0!B0!B1'B2-B2-B49B49B6FB7LB5?B7LB=qB>wB?}B@�B@�BB�BA�BD�BI�BJ�BM�BO�BQ�BQ�BR�BS�BVBXBYBYBYB\)B`BBbNBaHBaHBhsBl�Bm�Bn�Bo�Bp�Bq�Br�Br�Bs�Bs�Bt�Bu�By�B|�B~�B� B�B�B�B�B�7B�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�FB�RB�XB�dB�jB�jB�qBBƨBǮBɺB��B��B��B�B�B�/B�;B�HB�ZB�fB�mB�B�B�B��B��B��B	  B	B	B	%B	1B	DB	JB	JB	JB	VB	PB	VB	\B	bB	oB	hB	VB	
=B	DB	PB	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	+B	,B	,B	1'B	6FB	9XB	;dB	<jB	=qB	>wB	?}B	A�B	B�B	C�B	D�B	F�B	J�B	L�B	O�B	Q�B	VB	VB	T�B	W
B	\)B	_;B	cTB	ffB	gmB	gmB	gmB	hsB	jB	o�B	{�B	�B	�B	�B	�+B	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	��B	�sB	��B
B
VB
�B
�B
.B
49B
9XB
A�B
N�B
T�B
ZB
`BB
gmB
m�B
o�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B^B_&B^ B]BZBYB?eB$�BWB.B�B��B�BX�BH�B2BiBB��B��B�%B��BȚB�QB�B��B��B��B��B��B�ABnzB\BN�BApB/B%�B�B7B�B
��B
�EB
�B
ˬB
�pB
�GB
�B
��B
��B
��B
�yB
�5B
�B
�B
��B
}�B
q�B
eGB
S�B
72B
�B
B	��B	�DB	� B	��B	;B	ǚB	ĊB	�tB	�UB	�8B	�B	��B	�bB	�B	� B	�B	z�B	u�B	q�B	lxB	eMB	`/B	]B	[B	YB	T�B	N�B	E�B	?mB	5.B	+�B	�B		)B��B��B��B��B��B��B��B��B��B�B�B�uB�eB�QB�QB�OB�AB�;B�2B�!B�B�	B�B��B��B��B˹BʴBɭBȥBȦBƘBB�qB�UB�>B�%B�B�
B��B��B��B��B��B��B��B�{B�hB�WB�0B�B�B}�B|�B|�B{�Bx�Bt�Bq�Bo�Bn�Bn�Bm�BkzBjrBgaBbDB\BYBXBXBU�BS�BQ�BP�BP�BO�BM�BL�BJ�BH�BG�BF�BE�BD�BC�BB�BAB?tB;[B9NB8HB7CB7CB7@B55B4/B1 B0B0B1B2$B3*B2$B2B1B1B0B/B.	B+�B*�B*�B)�B(�B(�B(�B'�B'�B&�B%�B%�B$�B"�B!�B�B�B�B�B�ByB�B�B�B�B�B�B�B�B�B!�B"�B#�B(�B'�B+�B.	B.B0B0B1B2#B2!B4.B4,B68B7?B5/B7>B=eB>jB?qB@uB@wBB�BA|BD�BI�BJ�BM�BO�BQ�BQ�BR�BS�BU�BXBYBYBYB\B`3Bb>Ba:Ba:BhbBlzBm�Bn�Bo�Bp�Bq�Br�Br�Bs�Bs�Bt�Bu�By�B|�B~�B�B��B�B� B�B�#B�EB�cB�nB�|B�yB��B��B��B��B��B��B��B��B��B�B�4B�=B�AB�PB�UB�VB�\B�xBƐBǙBɤBʭB��B��B�B�B�B�$B�2B�FB�QB�WB�jB�B�B��B��B��B��B	B	B	B	B	,B	1B	2B	2B	>B	8B	?B	DB	KB	VB	QB	>B	
$B	,B	9B	8B	OB	nB	tB	uB	sB	nB	zB	�B	�B	�B	�B	�B	 �B	!�B	$�B	%�B	*�B	+�B	+�B	1B	6,B	9=B	;KB	<QB	=WB	>\B	?eB	AoB	BvB	C}B	D�B	F�B	J�B	L�B	O�B	Q�B	U�B	U�B	T�B	V�B	\B	_"B	c8B	fKB	gPB	gRB	gQB	hXB	jbB	o�B	{�B	��B	��B	�B	�B	�3B	�IB	�QB	�ZB	�^B	�iB	�iB	�sB	�oB	�qB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�TB	��B
 B
4B
�B
�B
-�B
4B
96B
AgB
N�B
T�B
Y�B
`B
gJB
mnB
o|B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20151229031614    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151229031614  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151229031614  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                