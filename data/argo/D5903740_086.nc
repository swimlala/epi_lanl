CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-24T21:06:37Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
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
_FillValue                 �  Kd   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  UD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ](   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20140724210637  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  4055_7112_086                   2C  D   APEX                            5374                            041511                          846 @�"7.�1   @�#j��@:�1&�x��d^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  BffB��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�L�D�I�D�ٚD�fD�P D�y�D���D��D�L�D���D���D�fD�FfD�i�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�A�Q�B(�B	(�B�\BB!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2c�C4c�C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�)Dy��D�D�VD�R�D���D��D�YHD���D��D�D�VD��D��D��D�O�D�r�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�9XA�7LA�7LA�7LA�5?A�=qA�;dA�/A���A�|�A���A��AȼjA���A�-AÍPA�bA��A���A�Q�A�%A�9XA���A���A��A�A�bA�A��A��A��A�1'A�ĜA�\)A���A�r�A�\)A��-A�5?A�S�A��A�
=A���A���A���A��A���A�bA���A�M�A���A��A��A��7A�`BA�-A��A��A�VA�+A�ȴA�`BA�  A��jA�p�A���A�t�A�  A�E�A��wA���A�G�A�dZA�%A��uA�=qA�VA��jA�M�A�-A���A���A��A�1A�|�A�oA��9A��uA�M�A���A�O�A� �A���A��HA���A���A�"�A��RA�A��A��A���A�A�+A�C�A��7A��7A�1A��!A��yA�(�A�9XA�Q�A��A�n�AK�A|�A{|�A{S�A{/A{�A{
=Az�Aw��At�Aq��Ap5?An��An�+AmAm7LAl1Ah��Af�Ac�hA`�A^n�A]�#A]��A]33A[�TAZz�AZ �AY��AYhsAY
=AX��AW�
AV�!AU�AS`BAR��ARz�AR(�AQ�AP�HAP�DAO�wAN��AM�^AL{AJĜAJ$�AI|�AH�AH$�AF�AE�AD�AB�A@$�A?VA>�9A>1A;�#A7|�A5��A5�A4~�A4JA3��A3
=A25?A1�A/�A/33A.�/A-�A,�jA,��A,ffA+K�A*��A*�A)��A)&�A(JA&ȴA&-A%dZA$�/A$�RA$A#;dA"�A"VA!�TA!�-A!&�A JA��A7LA��An�AVA{A��A�A�A�A&�A$�A��Ar�AI�A�A�uA��AE�Ax�A�\A��A
=AAO�A�A
�yA
A�A	�A	ƨA	��A	�7A	C�A9XA��A�!A �A�^A�A^5A�mAXA ��@�33@���@�bN@���@�S�@�{@��`@���@�A�@�"�@�n�@��#@�9X@���@�P@�@�K�@��y@���@�{@�7L@�z�@��;@�S�@�|�@�hs@���@�\)@�p�@؛�@��m@׍P@�K�@և+@�$�@��#@ԛ�@�ȴ@���@�(�@϶F@ϕ�@��@�ȴ@�n�@��@��@�j@ˮ@�~�@��@ɑh@�9X@��@��@�C�@�ȴ@�@�l�@��+@���@�Ĝ@��@�r�@�9X@���@�E�@���@�&�@��@��F@���@���@���@��w@�t�@��R@�$�@���@�9X@��@��9@�S�@��@�/@��`@��@�bN@�(�@�  @��@��
@��w@���@�dZ@�K�@�
=@�=q@��7@���@�33@���@��#@���@�r�@�A�@�(�@��@�1@�  @�|�@���@��@���@�@��^@��-@���@�hs@��u@��;@��@�S�@�@��@�@���@�`B@���@�I�@��P@��@��!@�v�@�V@�-@�J@��@��T@��T@���@�?}@��D@��@�E�@��h@��@��@��@�V@��u@� �@�  @���@���@��
@��P@�l�@�;d@��R@�-@��#@��@��@�bN@�  @��@�K�@�
=@���@�~�@�M�@�$�@�@��@��-@���@�`B@�?}@��@�Ĝ@���@�r�@�9X@�1@���@��R@��@���@�`B@�G�@���@��u@�1@��P@�l�@�"�@�
=@��y@�ȴ@��!@��\@�v�@���@���@��-@��h@���@��`@��/@���@��/@��/@���@��j@��D@�bN@��@l�@}�@}��@}��@}�h@}�h@}�h@}�h@}�h@}�h@}`B@}/@|�D@|Z@{�m@{��@v�R@n�+@d��@]V@Xb@R��@L9X@G;d@?K�@5�T@+S�@#ƨ@V@1'@z�@�#@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�9XA�7LA�7LA�7LA�5?A�=qA�;dA�/A���A�|�A���A��AȼjA���A�-AÍPA�bA��A���A�Q�A�%A�9XA���A���A��A�A�bA�A��A��A��A�1'A�ĜA�\)A���A�r�A�\)A��-A�5?A�S�A��A�
=A���A���A���A��A���A�bA���A�M�A���A��A��A��7A�`BA�-A��A��A�VA�+A�ȴA�`BA�  A��jA�p�A���A�t�A�  A�E�A��wA���A�G�A�dZA�%A��uA�=qA�VA��jA�M�A�-A���A���A��A�1A�|�A�oA��9A��uA�M�A���A�O�A� �A���A��HA���A���A�"�A��RA�A��A��A���A�A�+A�C�A��7A��7A�1A��!A��yA�(�A�9XA�Q�A��A�n�AK�A|�A{|�A{S�A{/A{�A{
=Az�Aw��At�Aq��Ap5?An��An�+AmAm7LAl1Ah��Af�Ac�hA`�A^n�A]�#A]��A]33A[�TAZz�AZ �AY��AYhsAY
=AX��AW�
AV�!AU�AS`BAR��ARz�AR(�AQ�AP�HAP�DAO�wAN��AM�^AL{AJĜAJ$�AI|�AH�AH$�AF�AE�AD�AB�A@$�A?VA>�9A>1A;�#A7|�A5��A5�A4~�A4JA3��A3
=A25?A1�A/�A/33A.�/A-�A,�jA,��A,ffA+K�A*��A*�A)��A)&�A(JA&ȴA&-A%dZA$�/A$�RA$A#;dA"�A"VA!�TA!�-A!&�A JA��A7LA��An�AVA{A��A�A�A�A&�A$�A��Ar�AI�A�A�uA��AE�Ax�A�\A��A
=AAO�A�A
�yA
A�A	�A	ƨA	��A	�7A	C�A9XA��A�!A �A�^A�A^5A�mAXA ��@�33@���@�bN@���@�S�@�{@��`@���@�A�@�"�@�n�@��#@�9X@���@�P@�@�K�@��y@���@�{@�7L@�z�@��;@�S�@�|�@�hs@���@�\)@�p�@؛�@��m@׍P@�K�@և+@�$�@��#@ԛ�@�ȴ@���@�(�@϶F@ϕ�@��@�ȴ@�n�@��@��@�j@ˮ@�~�@��@ɑh@�9X@��@��@�C�@�ȴ@�@�l�@��+@���@�Ĝ@��@�r�@�9X@���@�E�@���@�&�@��@��F@���@���@���@��w@�t�@��R@�$�@���@�9X@��@��9@�S�@��@�/@��`@��@�bN@�(�@�  @��@��
@��w@���@�dZ@�K�@�
=@�=q@��7@���@�33@���@��#@���@�r�@�A�@�(�@��@�1@�  @�|�@���@��@���@�@��^@��-@���@�hs@��u@��;@��@�S�@�@��@�@���@�`B@���@�I�@��P@��@��!@�v�@�V@�-@�J@��@��T@��T@���@�?}@��D@��@�E�@��h@��@��@��@�V@��u@� �@�  @���@���@��
@��P@�l�@�;d@��R@�-@��#@��@��@�bN@�  @��@�K�@�
=@���@�~�@�M�@�$�@�@��@��-@���@�`B@�?}@��@�Ĝ@���@�r�@�9X@�1@���@��R@��@���@�`B@�G�@���@��u@�1@��P@�l�@�"�@�
=@��y@�ȴ@��!@��\@�v�@���@���@��-@��h@���@��`@��/@���@��/@��/@���@��j@��D@�bN@��@l�@}�@}��@}��@}�h@}�h@}�h@}�h@}�h@}�h@}`B@}/@|�D@|Z@{�m@{��@v�R@n�+@d��@]V@Xb@R��@L9X@G;d@?K�@5�T@+S�@#ƨ@V@1'@z�@�#@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBF�BF�BF�BE�BE�BF�BF�BF�BG�BM�BM�BC�B49B
=B  B��B�yB�#BȴB�-B��B�uB�\B�VB�=B�+B�B}�Bv�Bt�Bp�BffBYBS�BO�BH�BF�BD�B?}B9XB2-B.B'�B#�B�BuBVBDB1BBB��B��B��B�B�B�B�B�TB�TB�HB�)B��B��BĜB�qB�3B�B��B��B�VB�DB�Bw�Bq�BjBe`B`BB[#BS�BP�BM�BG�BD�B>wB7LB/B)�B&�B!�B�BbBJB	7B1BBB��B�B�HB��B��B��B��B�Bp�B`BBH�B(�BDB
��B
�sB
��B
��B
�B
�B
cTB
J�B
A�B
?}B
=qB
<jB
:^B
0!B
�B
B	�yB	�/B	��B	��B	ȴB	ÖB	�RB	��B	�bB	�B	r�B	iyB	gmB	e`B	bNB	]/B	W
B	T�B	S�B	Q�B	P�B	M�B	I�B	D�B	<jB	5?B	33B	1'B	1'B	.B	,B	(�B	%�B	 �B	�B	hB	DB		7B	%B	B��B��B��B�B�sB�TB�;B�/B�B��BǮBŢBÖB��B��B�}B�qB�dB�RB�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�PB�JB�=B�1B�+B�B�B� B~�B|�By�Bv�Bs�Bp�Bm�BjBgmBcTB`BB^5B]/B[#BYBT�BQ�BN�BL�BI�BG�BE�BB�BA�BA�B?}B>wB=qB=qB<jB;dB9XB49B0!B.B-B,B)�B(�B&�B%�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBoB\B
=BDB
=BDBDBDBDBDBDBDBDB
=B
=BJBVBbBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B"�B$�B&�B)�B+B)�B)�B/B0!B2-B33B49B7LB;dB;dB<jBB�BB�BC�BD�BD�BF�BJ�BO�BS�BXB[#B\)B\)B]/B^5B^5B_;B_;B_;B`BB`BB`BB`BBcTBe`BjBr�Bu�Bz�B� B�B�B�B�%B�%B�B�=B�uB��B��B��B��B��B��B��B��B��B��B�B�B�?B�FB�LB�RB�^B�qB��BĜBŢBŢBƨBƨBǮBȴBȴBȴB��B��B�B�B�ZB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	+B	1B	DB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	'�B	)�B	,B	.B	0!B	1'B	33B	7LB	9XB	;dB	<jB	<jB	=qB	?}B	B�B	E�B	F�B	H�B	I�B	J�B	K�B	N�B	P�B	Q�B	XB	ZB	[#B	\)B	aHB	cTB	cTB	cTB	cTB	cTB	cTB	dZB	e`B	ffB	iyB	k�B	p�B	q�B	q�B	q�B	q�B	q�B	p�B	p�B	p�B	q�B	q�B	s�B	t�B	u�B	v�B	�=B	�B	��B	�NB	�B
  B
JB
�B
#�B
2-B
@�B
I�B
R�B
YB
^5B
bNB
e`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BF�BF�BF�BE�BE�BF�BF�BF�BG�BM�BM�BC�B4+B
0B��B��B�iB�BȢB�B��B�_B�FB�CB�*B�B��B}�Bv�Bt�Bp�BfOBYBS�BO�BH�BF�BD�B?eB9BB2B-�B'�B#�B�B]B=B)BBB�B��B��B��B�B�B�B�nB�<B�;B�/B�B��B˱BĂB�UB�B��B��B�vB�<B�(B�Bw�Bq�BjdBeCB`*B[	BS�BP�BM�BG�BD�B>\B71B.�B)�B&�B!�B�BEB0B	BBB �B��B�B�,B��BʤB�lB��B�Bp�B`%BH�B(�B)B
��B
�VB
��B
�iB
��B
�B
c;B
J�B
AqB
?cB
=XB
<RB
:GB
0
B
�B
 �B	�cB	�B	��B	��B	ȠB	�B	�?B	��B	�QB	��B	r�B	ihB	gYB	eMB	b<B	]B	V�B	T�B	S�B	Q�B	P�B	M�B	I�B	D�B	<YB	5/B	3#B	1B	1B	.B	+�B	(�B	%�B	 �B	�B	[B	6B		'B	B	B��B��B��B�B�dB�FB�.B�!B�	B��BǠBŔBÉB�|B�vB�pB�cB�YB�EB�&B�B�B��B��B��B��B��B��B��B��B��B��B��B�tB�pB�jB�]B�RB�GB�<B�0B�(B�"B�B��B�B~�B|�By�Bv�Bs�Bp�Bm�BjvBgeBcKB`7B^+B]$B[BYBT�BQ�BN�BL�BI�BG�BE�BB�BA�BA~B?tB>oB=iB=iB<^B;[B96B40B0B.
B-B+�B)�B(�B&�B%�B"�B!�B �B�B�B�B�BzBuBnB�B�BcBbBdBcBvB]BxBXBXBmBQBLBQB
B8B
B:BBB!BBB:B9B
B
B@B1B>BGBeBjBiBiBUBUBtBwBhB�B�B�B�B�B�B�B�B"�B$�B&�B)�B*�B)�B)�B/B0B2"B3&B4.B7?B;WB;XB<[BB�BB�BC�BD�BD�BF�BJ�BO�BS�BXB[B\B\B] B^&B^$B_*B_-B_-B`1B`5B`3B`2BcFBeQBjoBr�Bu�Bz�B�B�B�
B�B�B�B�B�+B�aB��B��B��B��B��B��B��B��B��B��B��B��B�+B�2B�6B�=B�LB�\B�sBćBŎBŋBƓBƒBǖBȞBȜBȞBʬB��B��B��B�DB�oB�~B�B�B�B�B��B��B��B��B��B��B��B��B	 �B	B	B	B	,B	OB	bB	kB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	'�B	)�B	+�B	-�B	0B	1B	3B	72B	9<B	;KB	<QB	<PB	=UB	?bB	BtB	E�B	F�B	H�B	I�B	J�B	K�B	N�B	P�B	Q�B	W�B	ZB	[	B	\B	a-B	c:B	c:B	c:B	c;B	c;B	c9B	d?B	eEB	fHB	i]B	kkB	p�B	q�B	q�B	q�B	q�B	q�B	p�B	p�B	p�B	q�B	q�B	s�B	t�B	u�B	v�B	�"B	��B	̮B	�+B	�B	��B
)B
gB
#�B
2B
@bB
I�B
R�B
X�B
^B
b.B
e<1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140724210637    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140724210637  QCP$                G�O�G�O�G�O�8FB7E           AO  ARGQQCPL                                                                    20140724210637  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                