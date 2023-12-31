CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-18T19:16:41Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151018191641  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               NA   AO  5286_8897_078                   2C  D   APEX                            6531                            072314                          846 @�w�5�x1   @�w��-��@2vȴ9X�c3
=p��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    NA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BPffBW33B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy� D�fD�L�D��fD��fD� D�S3D�c3D���D��D�<�D�s3DǼ�D��D�VfD�vfD�ɚD�3D�@ D�|�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A�B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BP�GBW�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.C0�C2�C4�C6�C88RC:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtt{DyǮD�=D�P�D��=D��=D��D�W
D�g
D���D��D�@�D�w
D���D��D�Z=D�z=D��qD�
D�C�D�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�(�A�(�A�+A�-A�-A�-A�-A�&�A�"�A��A��A���A���Aݕ�A�r�A�A�A�|�A�K�A�7LA��mA�Aۙ�A�"�AځA�  A��A��A���A�~�A�9XA�n�A�  A̕�A�A�{A�1'Aɣ�A�v�A�9XAǲ-A�A�A�C�AĴ9A�n�A²-A���A�t�A�1'A�p�A�A��A��A�/A�M�A�v�A�ffA���A���A�{A�=qA��mA�A��FA�~�A��A��#A�`BA�33A��jA��^A��A�C�A�t�A�"�A���A���A�/A���A�bNA��yA��wA�oA�ZA�oA�v�A�ZA���A��A�O�A�ZA�A��A�VA���A���A�/A��^A�K�A��`A�I�A�A�M�A�t�A��A�x�AXA{x�Ax��AvAu�As�7Ao�mAm�Al$�Ad�`Ac`BAb�DA`�`A`jA`5?A_�A_p�A^�A\^5AW��AUhsAS�AR��AR�+AQ�mAPVAN9XAL��AJ~�AHQ�AF�/AE�wAC��A@��A=p�A< �A;dZA9�A8��A7�wA7"�A6�jA6ZA5�mA5hsA5VA4 �A3O�A2�A1�A0jA-33A*��A)�A(  A'x�A&A$�9A$bA"�A!&�A��A�A�A|�At�A�A�+A\)A��AK�A�;AG�A��A��At�A�\A\)A^5AhsAA�jA�!Ar�A��AXAȴA
$�A��A1A�AA�A1A��A�7A�AA�A|�A��A�A�^A �A��A�`AȴAffA�A�hA\)A�`A��A
=A��A�^A\)A�/AȴA�AhsAhsA;dAS�A(�A|�A n�@���@���@�/@�@��^@��D@�^5@���@���@�;d@�v�@�p�@���@�Ĝ@�F@���@�7L@�@�Q�@�9X@�ƨ@�S�@�33@�ȴ@�=q@��T@�%@���@�|�@��@�$�@�@��`@�A�@��;@㝲@�o@�-@�G�@���@��@��y@ާ�@ݑh@ܛ�@�@��@�/@�X@�?}@؛�@׾w@ׅ@�C�@�o@ְ!@�M�@�@Ӿw@���@��@�33@�"�@��@��@�j@��@��@�l�@���@��`@��m@ˍP@ʇ+@�x�@�G�@�V@�I�@��@��m@ǝ�@��@��-@�V@��T@���@��w@���@�@�j@�`B@���@�V@�v�@�z�@�(�@�+@� �@��w@���@���@���@�A�@�@��\@�~�@�V@��@��^@��u@���@�V@��m@�$�@���@�"�@��\@�ff@�-@��^@��@�M�@��@��^@�@�C�@�^5@��y@�o@���@��R@�v�@��@���@��@�9X@���@���@��R@���@�~�@�V@�-@��@��@�Z@�Q�@��F@���@���@�5?@�Q�@��@�v�@��h@��@��9@�Z@�I�@�Ĝ@���@�Ĝ@�1@��
@���@�33@��R@�
=@���@�%@�I�@��;@���@�o@�o@��!@�ff@��#@�@�@��^@���@��h@���@���@��^@�@��D@�|�@�C�@��y@���@�n�@�M�@�@���@�p�@�O�@�?}@���@�V@�%@��P@���@���@��j@�Q�@� �@�ƨ@���@��;@�r�@�O�@�$�@��@��y@�v�@�`B@�/@�V@�%@�G�@��@�Ĝ@�b@�b@�b@�1@�  @�|�@�t�@�+@�C�@��@��+@�ȴ@�ȴ@��+@�{@�/@��w@�-@��-@��@�&�@���@�r�@� �@��m@�1@�bN@���@�l�@�33@���@�5?@�@��!@�(�@x��@n��@dj@\�@S�m@N$�@F�@@��@;"�@5V@/�w@*�@$�@�R@�`@�h@�@�-@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�+A�(�A�(�A�+A�-A�-A�-A�-A�&�A�"�A��A��A���A���Aݕ�A�r�A�A�A�|�A�K�A�7LA��mA�Aۙ�A�"�AځA�  A��A��A���A�~�A�9XA�n�A�  A̕�A�A�{A�1'Aɣ�A�v�A�9XAǲ-A�A�A�C�AĴ9A�n�A²-A���A�t�A�1'A�p�A�A��A��A�/A�M�A�v�A�ffA���A���A�{A�=qA��mA�A��FA�~�A��A��#A�`BA�33A��jA��^A��A�C�A�t�A�"�A���A���A�/A���A�bNA��yA��wA�oA�ZA�oA�v�A�ZA���A��A�O�A�ZA�A��A�VA���A���A�/A��^A�K�A��`A�I�A�A�M�A�t�A��A�x�AXA{x�Ax��AvAu�As�7Ao�mAm�Al$�Ad�`Ac`BAb�DA`�`A`jA`5?A_�A_p�A^�A\^5AW��AUhsAS�AR��AR�+AQ�mAPVAN9XAL��AJ~�AHQ�AF�/AE�wAC��A@��A=p�A< �A;dZA9�A8��A7�wA7"�A6�jA6ZA5�mA5hsA5VA4 �A3O�A2�A1�A0jA-33A*��A)�A(  A'x�A&A$�9A$bA"�A!&�A��A�A�A|�At�A�A�+A\)A��AK�A�;AG�A��A��At�A�\A\)A^5AhsAA�jA�!Ar�A��AXAȴA
$�A��A1A�AA�A1A��A�7A�AA�A|�A��A�A�^A �A��A�`AȴAffA�A�hA\)A�`A��A
=A��A�^A\)A�/AȴA�AhsAhsA;dAS�A(�A|�A n�@���@���@�/@�@��^@��D@�^5@���@���@�;d@�v�@�p�@���@�Ĝ@�F@���@�7L@�@�Q�@�9X@�ƨ@�S�@�33@�ȴ@�=q@��T@�%@���@�|�@��@�$�@�@��`@�A�@��;@㝲@�o@�-@�G�@���@��@��y@ާ�@ݑh@ܛ�@�@��@�/@�X@�?}@؛�@׾w@ׅ@�C�@�o@ְ!@�M�@�@Ӿw@���@��@�33@�"�@��@��@�j@��@��@�l�@���@��`@��m@ˍP@ʇ+@�x�@�G�@�V@�I�@��@��m@ǝ�@��@��-@�V@��T@���@��w@���@�@�j@�`B@���@�V@�v�@�z�@�(�@�+@� �@��w@���@���@���@�A�@�@��\@�~�@�V@��@��^@��u@���@�V@��m@�$�@���@�"�@��\@�ff@�-@��^@��@�M�@��@��^@�@�C�@�^5@��y@�o@���@��R@�v�@��@���@��@�9X@���@���@��R@���@�~�@�V@�-@��@��@�Z@�Q�@��F@���@���@�5?@�Q�@��@�v�@��h@��@��9@�Z@�I�@�Ĝ@���@�Ĝ@�1@��
@���@�33@��R@�
=@���@�%@�I�@��;@���@�o@�o@��!@�ff@��#@�@�@��^@���@��h@���@���@��^@�@��D@�|�@�C�@��y@���@�n�@�M�@�@���@�p�@�O�@�?}@���@�V@�%@��P@���@���@��j@�Q�@� �@�ƨ@���@��;@�r�@�O�@�$�@��@��y@�v�@�`B@�/@�V@�%@�G�@��@�Ĝ@�b@�b@�b@�1@�  @�|�@�t�@�+@�C�@��@��+@�ȴ@�ȴ@��+@�{@�/@��w@�-@��-@��@�&�@���@�r�@� �@��m@�1@�bN@���@�l�@�33@���@�5?G�O�@��!@�(�@x��@n��@dj@\�@S�m@N$�@F�@@��@;"�@5V@/�w@*�@$�@�R@�`@�h@�@�-@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bx�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bt�Br�Bo�Bm�BiyB`BBbNBdZBbNBaHBcTB�=B�-B�FB��B�B��B+BJB'�B>wBM�BW
B]/BaHBcTBe`BgmB}�Bz�B�B�B}�B�B�DB��B�B�3BB��B��B�/B�`B�B�B��B��B  B  B��B��B��B�B�B�B�yB�sB�mB�BB�B��BB�!B��B�hB�B{�Bo�BVB2-B�BB�B�fB�
B��BĜB�-B��B~�BJ�B(�B
��B
�sB
��B
�LB
��B
��B
�DB
�B
{�B
q�B
hsB
P�B
0!B
JB	��B	�fB	�B	�fB	��B	�dB	��B	v�B	iyB	cTB	jB	jB	jB	hsB	dZB	]/B	N�B	5?B	,B	$�B	 �B	�B	�B	uB	B��B�B�TB�)B�
B��BȴB�qB�XB�RB�LB�?B�3B�'B�!B�B�B�B��B��B��B��B��B��B�1Bz�Bw�Bu�Bt�Bu�Bt�Br�Bo�Bp�Bp�Bv�B�B�%B�%B�%B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�+Bx�B� B�+B�oB�uB�uB�oB��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B�B�B��B	+B	VB	VB	VB	�B	�B	�B	�B	$�B	 �B	�B	�B	 �B	�B	�B	�B	�B	hB	oB	�B	uB	bB	bB	bB	uB	�B	�B	�B	�B	#�B	&�B	)�B	/B	33B	49B	6FB	5?B	6FB	7LB	:^B	@�B	A�B	B�B	C�B	C�B	B�B	B�B	B�B	?}B	=qB	@�B	D�B	G�B	E�B	K�B	I�B	E�B	B�B	I�B	J�B	K�B	N�B	O�B	M�B	L�B	L�B	K�B	K�B	K�B	L�B	L�B	M�B	Q�B	W
B	W
B	ZB	[#B	\)B	\)B	]/B	\)B	]/B	^5B	^5B	_;B	`BB	ffB	l�B	n�B	n�B	m�B	n�B	l�B	bNB	W
B	^5B	iyB	m�B	ffB	p�B	}�B	z�B	o�B	gmB	cTB	dZB	v�B	v�B	v�B	�B	�B	~�B	� B	�B	�%B	�JB	�PB	�PB	�PB	�JB	�JB	�DB	�=B	�B	{�B	u�B	q�B	p�B	o�B	n�B	m�B	m�B	r�B	u�B	}�B	|�B	�B	�JB	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�3B	�3B	�9B	�FB	�RB	�qB	ĜB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�/B	�NB	�5B	�)B	�)B	�)B	�/B	�/B	�;B	�HB	�NB	�NB	�HB	�NB	�TB	�`B	�`B	�HB	�5B	�)B	�B	�B	�#B	�/B	�BB	�TB	�sB	�B	��B
B
B
  B	��B	��B	��B	��B
  B
B
  B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
+B
bB
�B
#�B
+B
2-B
9XB
>wB
C�B
L�B
P�B
T�B
ZB
`BB
ffB
iyB
n�B
r�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Bx�Bx�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bt�Br�Bo�Bm�BioB`9BbEBdTBbFBaABcJB�7B�$B�=BʶB�B��B B?B'�B>lBM�BWB]#Ba>BcKBeXBgdB}�Bz�B��B�B}�B��B�:B��B��B�+BB��B��B�(B�XB�B�B��B��B��B��B��B��B��B�B�B�}B�pB�iB�cB�:B�B��BB�B��B�YB�	B{�Bo�BU�B2!B�B �B�B�ZB��B��BĐB�"B��B~�BJ�B(�B
��B
�mB
��B
�CB
��B
�xB
�;B
�B
{�B
q�B
hkB
P�B
0B
EB	��B	�`B	�B	�aB	��B	�cB	��B	v�B	izB	cUB	j�B	j~B	j�B	htB	dYB	]0B	N�B	5@B	,
B	$�B	 �B	�B	�B	zB	#B��B�B�YB�/B�B��BȹB�vB�`B�WB�TB�FB�:B�-B�(B�B�B�B�B��B��B��B��B��B�7Bz�Bw�Bu�Bt�Bu�Bt�Br�Bo�Bp�Bp�Bv�B�"B�,B�+B�,B�LB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�4Bx�B�B�3B�uB�zB�|B�uB��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B�B�B��B	-B	UB	WB	VB	B	�B	�B	�B	$�B	 �B	�B	�B	 �B	�B	�B	�B	�B	hB	rB	�B	tB	cB	fB	cB	sB	�B	�B	�B	�B	#�B	&�B	)�B	/B	34B	48B	6EB	5>B	6EB	7JB	:^B	@�B	A�B	B�B	C�B	C�B	B�B	B�B	B�B	?{B	=oB	@�B	D�B	G�B	E�B	K�B	I�B	E�B	B�B	I�B	J�B	K�B	N�B	O�B	M�B	L�B	L�B	K�B	K�B	K�B	L�B	L�B	M�B	Q�B	WB	WB	ZB	[B	\&B	\&B	]+B	\'B	]-B	^3B	^1B	_:B	`?B	faB	l�B	n�B	n�B	m�B	n�B	l�B	bHB	WB	^1B	ivB	m�B	fbB	p�B	}�B	z�B	o�B	ghB	cQB	dWB	v�B	v�B	v�B	� B	� B	~�B	�B	�B	�B	�BB	�IB	�NB	�JB	�CB	�DB	�=B	�9B	�B	{�B	u�B	q�B	p�B	o�B	n�B	m�B	m�B	r�B	u�B	}�B	|�B	� B	�DB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	� B	�B	�B	�B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�1B	�,B	�.B	�0B	�>B	�LB	�jB	ĕB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�FB	�/B	� B	�B	�%B	�(B	�(B	�3B	�AB	�DB	�EB	�@B	�FB	�KB	�YB	�UB	�AB	�/B	�"B	�B	�B	�B	�&B	�;B	�KB	�jB	�B	��B
 �B
B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
 �B
B
	B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
$B
YB
�B
#�B
*�B
2#B
9MB
>lB
C�B
L�B
P�B
T�B
ZB
`5B
f[B
imB
n�B
r�B
t�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151018191641    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151018191641  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151018191641  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                