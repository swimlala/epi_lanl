CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:49Z AOML 3.0 creation; 2016-06-01T00:08:19Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230849  20160531170819  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               TA   AO  4055_7112_084                   2C  D   APEX                            5374                            041511                          846 @�=�1   @���@;0 ě���d
fffff1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    TA   A   A   @�33@�  A   A   A@  A`  A���A�33A�33A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D�  D�<�D���D���D� D�C3D��fD��fD�3D�6fD���D��fD�3D�FfDږfD���D��fD�33D�|�D�S311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A$��AD��Ad��A��A��A��A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�BB(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=Chc�CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=Cvc�Cxc�CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm)Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�)Dy��D�)HD�FD���D��D�HD�L{D���D�ϮD�{D�?�D��D�߮D�{D�O�Dڟ�D��D���D�<{D�D�\{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yAɾwA�z�A�oA���A�&�A�E�A� �AüjA��PA�ȴA�%A�Q�A�ĜA���A�
=A���A�/A�S�A�v�A���A�9XA���A���A��+A�A�v�A��`A�jA�M�A��A��A��!A�l�A�(�A�\)A��A�;dA��wA�K�A�1A�ȴA��A�ZA�VA��!A�{A���A���A���A��A���A��uA�r�A�=qA���A��RA���A���A���A��9A���A��A��RA�~�A�=qA���A��DA�t�A�hsA�$�A���A��yA��/A���A��FA�z�A�`BA�A�A�33A��A�A��/A�ĜA��FA�|�A�-A��;A���A�`BA�S�A�I�A�A���A��jA���A���A��A�\)A�+A���A��/A���A���A���A��jA��^A��A���A��\A�ffA�5?A�(�A��A�A��wA���A�S�A��;A���A�|�A�r�A�hsA�^5A�?}A��/A�"�A��TA�~�A�O�A�ZA�ĜA��A��#A�VA��uA�9XA~�/Ay��Av�yAs�Am/Af�AdQ�Ac?}A`��A^n�A\^5AX=qAV�uAVjAVE�AU�-AU��AT��AQ%AN�AL�`AJ�AI��AH��AES�ACAB�AA��AA�hAA�PAAp�AAC�AAA@{A?|�A?S�A<��A;/A9��A733A6$�A4��A1x�A1�A0�A0ȴA0�A0v�A0I�A0�A/�mA/�^A/|�A.�A-S�A,��A+�FA+p�A+O�A+33A*��A*��A)�wA'��A$�A"��A!�FA �yA�FAl�A�A�RAz�AbNA$�A��AA��A�AhsA��AA�A�A
=AE�AE�A�wA��A|�AXA�A9XA�wA?}A��A��Av�A$�A�-A��A��AK�A��AoAbA
�A
5?A
bA	�A	�A	`BA	"�A�uA��A�yAjA�mA��A �H@�
=@�@�@��@�S�@�n�@�V@�p�@��m@���@�+@���@�1@��u@��@���@ٺ^@�C�@�hs@��/@�(�@�+@ҟ�@�E�@��@��@щ7@��@���@�Ĝ@�j@���@�l�@�M�@͡�@�%@̬@�A�@˕�@�
=@�v�@ȼj@Ƨ�@ř�@�@��m@��@��@�9X@��m@�dZ@�"�@���@���@�ȴ@���@�n�@�=q@���@�x�@�Ĝ@�ƨ@�l�@���@�7L@��u@�b@���@�+@��@��;@�S�@��\@��@��`@�\)@�$�@� �@�|�@��y@���@�=q@�5?@�-@�J@��@��#@��^@��7@�`B@�X@�G�@�/@��@�%@�Ĝ@�9X@�
=@��@�X@�V@��@��u@�z�@�Q�@�\)@��R@�J@���@�G�@��9@�(�@�1@�n�@�7L@��`@�z�@�1'@��@�Z@�Z@�I�@�I�@�9X@�(�@��@��@�\)@��@���@��+@�-@�hs@�bN@��@�+@�+@�33@�+@�\)@�;d@�
=@�@��y@���@��!@�V@�@��^@��@�X@��`@��9@���@�r�@�r�@�A�@�b@��@�\)@��@��\@�M�@�{@�@�x�@�G�@�&�@�V@��@�9X@��m@���@�o@�E�@�@���@�@��^@���@�X@�7L@�/@��@�Ĝ@�K�@�v�@�@��-@�x�@�O�@�%@�Ĝ@���@�A�@��;@��@��@�t�@�;d@��@���@���@�E�@�J@��#@��-@�p�@�%@�bN@� �@�1@�w@+@~��@~v�@~E�@~5?@~5?@~$�@~@}�T@}�@}?}@}V@|��@|�j@|��@|z�@|j@|(�@|1@v��@l��@e��@_�@Y��@R��@I��@Ahs@:~�@5`B@1X@*n�@'�@!��@1@K�@-@ȴ@I�@	7L@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yAɾwA�z�A�oA���A�&�A�E�A� �AüjA��PA�ȴA�%A�Q�A�ĜA���A�
=A���A�/A�S�A�v�A���A�9XA���A���A��+A�A�v�A��`A�jA�M�A��A��A��!A�l�A�(�A�\)A��A�;dA��wA�K�A�1A�ȴA��A�ZA�VA��!A�{A���A���A���A��A���A��uA�r�A�=qA���A��RA���A���A���A��9A���A��A��RA�~�A�=qA���A��DA�t�A�hsA�$�A���A��yA��/A���A��FA�z�A�`BA�A�A�33A��A�A��/A�ĜA��FA�|�A�-A��;A���A�`BA�S�A�I�A�A���A��jA���A���A��A�\)A�+A���A��/A���A���A���A��jA��^A��A���A��\A�ffA�5?A�(�A��A�A��wA���A�S�A��;A���A�|�A�r�A�hsA�^5A�?}A��/A�"�A��TA�~�A�O�A�ZA�ĜA��A��#A�VA��uA�9XA~�/Ay��Av�yAs�Am/Af�AdQ�Ac?}A`��A^n�A\^5AX=qAV�uAVjAVE�AU�-AU��AT��AQ%AN�AL�`AJ�AI��AH��AES�ACAB�AA��AA�hAA�PAAp�AAC�AAA@{A?|�A?S�A<��A;/A9��A733A6$�A4��A1x�A1�A0�A0ȴA0�A0v�A0I�A0�A/�mA/�^A/|�A.�A-S�A,��A+�FA+p�A+O�A+33A*��A*��A)�wA'��A$�A"��A!�FA �yA�FAl�A�A�RAz�AbNA$�A��AA��A�AhsA��AA�A�A
=AE�AE�A�wA��A|�AXA�A9XA�wA?}A��A��Av�A$�A�-A��A��AK�A��AoAbA
�A
5?A
bA	�A	�A	`BA	"�A�uA��A�yAjA�mA��A �H@�
=@�@�@��@�S�@�n�@�V@�p�@��m@���@�+@���@�1@��u@��@���@ٺ^@�C�@�hs@��/@�(�@�+@ҟ�@�E�@��@��@щ7@��@���@�Ĝ@�j@���@�l�@�M�@͡�@�%@̬@�A�@˕�@�
=@�v�@ȼj@Ƨ�@ř�@�@��m@��@��@�9X@��m@�dZ@�"�@���@���@�ȴ@���@�n�@�=q@���@�x�@�Ĝ@�ƨ@�l�@���@�7L@��u@�b@���@�+@��@��;@�S�@��\@��@��`@�\)@�$�@� �@�|�@��y@���@�=q@�5?@�-@�J@��@��#@��^@��7@�`B@�X@�G�@�/@��@�%@�Ĝ@�9X@�
=@��@�X@�V@��@��u@�z�@�Q�@�\)@��R@�J@���@�G�@��9@�(�@�1@�n�@�7L@��`@�z�@�1'@��@�Z@�Z@�I�@�I�@�9X@�(�@��@��@�\)@��@���@��+@�-@�hs@�bN@��@�+@�+@�33@�+@�\)@�;d@�
=@�@��y@���@��!@�V@�@��^@��@�X@��`@��9@���@�r�@�r�@�A�@�b@��@�\)@��@��\@�M�@�{@�@�x�@�G�@�&�@�V@��@�9X@��m@���@�o@�E�@�@���@�@��^@���@�X@�7L@�/@��@�Ĝ@�K�@�v�@�@��-@�x�@�O�@�%@�Ĝ@���@�A�@��;@��@��@�t�@�;d@��@���@���@�E�@�J@��#@��-@�p�@�%@�bN@� �@�1@�w@+@~��@~v�@~E�@~5?@~5?@~$�@~@}�T@}�@}?}@}V@|��@|�j@|��@|z�@|j@|(�@|1@v��@l��@e��@_�@Y��@R��@I��@Ahs@:~�@5`B@1X@*n�@'�@!��@1@K�@-@ȴ@I�@	7L@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�BuBhBVB
=BB�B�sB�NB��B�B��B�%Bw�BbNBF�BA�B<jB2-B-B(�B"�B�BhBVBDB1B+B%BBB��B��B��B�B�B�B�mB�`B�TB�NB�5B�)B�
B��B��B��B��B��B��B��B��B��B��BŢBÖB��B�?B�?B�RB�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�\B�VB�DB�7B�+B�B�B{�Bv�Bq�Bl�Bl�Bk�BhsBffBe`BcTBbNBaHB_;B\)BXBVBT�BS�BR�BQ�BQ�BP�BO�BM�BJ�BG�BF�BE�BD�B>wB<jB5?B)�B"�B �B�B�B�B�BPB��B�B�fBǮB��B_;BC�B1B
�BB
�B
{�B
jB
?}B
%�B
%B	��B	��B	�hB	�1B	x�B	hsB	YB	F�B	@�B	@�B	>wB	<jB	:^B	33B	%�B	�B	�B	oB	PB	+B��B��B��B�B�B�B�B�B�B�B�sB�ZB�5B�B��B��BɺBB�dB�XB�XB�RB�RB�LB�FB�?B�?B�9B�-B�B��B��B��B��B��B��B��B��B��B�uB�DB�+B�B�B�B� B~�B~�B~�B}�B}�B|�B|�B{�B{�Bz�Bx�Bv�Bt�Br�Bn�Bk�BiyBiyBhsBgmBe`BdZBbNBaHB_;B_;B^5B\)BZBT�BO�BK�BH�BD�BA�B?}B=qB=qB<jB;dB:^B9XB6FB49B2-B0!B+B$�B!�B�B�B�B�B�B�B{BoB\BVBJB
=B
=B
=B
=B	7B
=BDBPBPBPBVB\B\B\B\B\BbBbBbBbBbBbBoBoBuBuBuB{B{B{B�B�B�B�B�B!�B#�B$�B$�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B)�B)�B+B0!B2-B49B6FB8RBD�BL�BL�BN�BO�BR�B]/BbNBjBk�Bl�Bm�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bs�Bw�B{�B}�B~�B~�B� B�B� B�B�B�+B�7B�=B�VB�uB�uB��B��B��B�B�B�!B�B�B�B�B�-B�3B�FB�LB�FB�RB�}BĜBƨB��B��B�)B�;B�;B�;B�;B�/B�5B�BB�BB�HB�HB�HB�ZB�`B�sB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	+B		7B	DB	JB	PB	VB	hB	�B	�B	�B	�B	$�B	(�B	,B	-B	.B	/B	33B	49B	5?B	5?B	6FB	?}B	C�B	E�B	G�B	H�B	I�B	K�B	M�B	M�B	P�B	S�B	VB	T�B	W
B	YB	ZB	ZB	\)B	_;B	aHB	bNB	bNB	cTB	ffB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	s�B	s�B	u�B	v�B	w�B	x�B	x�B	y�B	y�B	y�B	z�B	{�B	�VB	�!B	��B	�)B	�B	��B
bB
�B
+B
2-B
6FB
A�B
F�B
N�B
T�B
\)B
bNB
gmB
jB
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B}BiBUBJB
-B B�B�bB�<B��B�	B�lB�Bw�Bb7BF�BApB<RB2B,�B(�B"�BwBQB?B(BBBB B�B��B��B��B�B�|B�nB�QB�HB�7B�4B�B�B��B��B��B��B��B��B��B��B��B��B˰BŉB�zB�jB�#B�%B�8B��B��B��B��B��B��B��B��B��B��B�vB�sB�kB�kB�`B�[B�GB�DB�9B�%B�B�B�B��B{�Bv�Bq�BlqBloBkiBhVBfKBeFBc8Bb3Ba-B_!B\BW�BU�BT�BS�BR�BQ�BQ�BP�BO�BM�BJ�BG�BF�BE�BD�B>]B<NB5!B)�B"�B �B�B�B�BrB3B��B�B�IBǔB�zB_BC|BB
�*B
��B
{�B
jfB
?dB
%�B
B	��B	��B	�VB	�B	x�B	hbB	YB	F�B	@uB	@rB	>fB	<ZB	:OB	3"B	%�B	�B	~B	^B	AB	B��B��B��B�B�B�B�B�B�B�qB�dB�KB�(B�B��B��BɭBB�WB�JB�JB�CB�FB�>B�9B�5B�4B�-B�!B�B��B��B��B��B��B��B��B��B��B�hB�9B�B�B�B��B�B~�B~�B~�B}�B}�B|�B|�B{�B{�Bz�Bx�Bv�Bt�Br�Bn�Bk|BirBinBhiBgbBeTBdPBbEBa?B_2B_0B^(B\BZBT�BO�BK�BH�BD�BA�B?tB=gB=jB<bB;YB:SB9OB6>B4B2$B0B*�B$�B!�B�B�B�BuB�BcBWBJB6B2B&B
B
B
B
B	B
5BB*B*B,B2BRB5BQBRBSBXBVB=BXBZBZBeBfBjBkBiBpBqBpB[B�B�B�B�B!�B#�B$�B$�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B)�B)�B*�B0B2B4,B68B8EBD�BL�BL�BN�BO�BR�B]Bb?BjnBkuBlzBm�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bq�Bs�Bw�B{�B}�B~�B~�B�B��B�B��B�B�B�%B�+B�DB�cB�cB��B��B��B��B�B�B�B�B�B�	B�B�B�3B�9B�1B�;B�gBĉBƔB˲B��B�B�%B�%B�%B�%B�B�!B�+B�+B�3B�0B�2B�DB�IB�]B�hB�tB��B�B�B��B��B��B��B��B��B��B	�B	B	B		 B	*B	2B	7B	>B	PB	hB	uB	�B	�B	$�B	(�B	+�B	,�B	-�B	/B	3B	4B	5%B	5&B	6+B	?cB	C}B	E�B	G�B	H�B	I�B	K�B	M�B	M�B	P�B	S�B	U�B	T�B	V�B	X�B	ZB	ZB	\B	_ B	a,B	b2B	b2B	c8B	fJB	kjB	loB	mvB	n}B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	s�B	s�B	u�B	v�B	w�B	x�B	x�B	y�B	y�B	y�B	z�B	{�B	�9B	�B	ʢB	�	B	�rB	��B
BB
�B
*�B
2
B
6%B
AgB
F�B
N�B
T�B
\B
b+B
gMB
j]B
o|B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708192016053117081920160531170819  AO  ARCAADJP                                                                    20140721230849    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230849  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230849  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170819  IP                  G�O�G�O�G�O�                