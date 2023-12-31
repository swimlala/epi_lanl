CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:08Z AOML 3.0 creation; 2016-08-07T21:51:10Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221408  20160807145110  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               	A   AO  5287_9017_009                   2C  D   APEX                            6529                            072314                          846 @�y\��1   @� @2t�j~��c��E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    	A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�fD�@ D�y�D�� D�	�D�FfD�|�D�� D�fD�<�D�ffDǶfD��fD�33DږfD��D�fD�I�D�\�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B��{B�ǮB�ǮB�ǮB̔{B�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�{B�ǮB�ǮB�ǮC c�Cc�Cc�C}qC}qC
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C }qC"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CV}qCX}qCZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�\D�"�D�L{D��D��{D�D�R�D��HD��{D��D�IHD�r�D���D��D�?�Dڢ�D��HD��D�VD�iHD�v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AݾwAݺ^Aݧ�A�ffA�\)A�&�A�ƨAܬAܧ�Aܥ�Aܣ�Aܡ�Aܝ�Aܝ�Aܛ�Aܙ�Aܕ�A܏\A܉7A܅A�|�A�x�A�r�A�n�A�jA�
=A�r�A�`BA�7LA���Aٛ�A�`BA�I�A�=qA�33A�%AؼjA�G�Aס�A�ZA�I�A�VA�XA�ĜA�+A�hsAҧ�A�ffA���AоwAϏ\A�x�A˥�A�Aǉ7A��HA�XA�jA�ĜA��A�M�A�&�A�-A��7A��A��A�^5A�A���A���A�C�A��RA���A�"�A�I�A��9A�A�E�A�ȴA�M�A��
A��A��wA��A��mA��7A���A�`BA�1'A�v�A���A��A~  AwS�Ap�An�Al�jAk7LAgx�Ad{Ab��A]|�AX�jAXZAV(�AS�hAR�AP��AM��AI��AG�TAF��ABQ�AA\)A>�HA;�wA:�A9XA5+A4r�A3��A2^5A0�A/oA.ffA,ĜA+dZA*^5A(��A&��A%ƨA$�DA$-A#��A#;dA"ȴA"A!?}A ��AoA �A/Az�AhsA|�A�HAA��A�TA�A�yAffAĜA|�AoA�!Az�A$�A��A�RA�A
=A
$�A�9A�^A;dAA�A�A�mA�AE�A�A��AE�AO�A �A Q�A =qA A�A r�A ffA �A33AG�A ~�A �A ĜA ��A �!A �DA bNA $�@���@�@��@�V@�+@�~�@��@���@���@�?}@�r�@� �@�1@��F@���@�@��/@���@�z�@�z�@�D@��u@�r�@�A�@�@��y@�-@��#@�h@�V@�z�@@�S�@��@��@�ff@�E�@��@�Q�@���@띲@�J@睲@�{@�G�@�V@�`B@�\@��@�x�@�7L@�j@�D@��@�x�@��@��`@�j@�9@�Z@�9X@�1'@�1@�S�@��H@�$�@�bN@۾w@���@�n�@��@��T@�x�@�1@֧�@�`B@�%@���@ԣ�@�j@�Q�@ӶF@ӍP@�|�@�t�@�t�@�;d@�ȴ@�5?@�@���@��@��@���@�`B@��@д9@�A�@�1@Ϯ@�C�@Ο�@���@�/@�Z@˾w@�+@��@�V@ɩ�@ə�@�/@��/@Ȭ@�Q�@�1@Ǯ@�\)@���@�ff@�J@Ų-@őh@���@��m@�"�@§�@��@���@���@�hs@��@�  @��F@�|�@�dZ@�+@��!@�^5@�$�@�hs@���@���@�bN@��F@�C�@�33@�
=@���@�n�@�V@�M�@�E�@�E�@�=q@��-@�(�@�K�@�ȴ@�=q@�{@��^@��D@��@��@���@��F@��P@�t�@�\)@��@�X@�V@��/@�1'@�\)@�"�@��+@�-@���@�`B@�?}@��@���@�9X@��m@��F@���@�l�@�@��!@��@�@��^@��7@�O�@�?}@�&�@�V@���@�Q�@���@�S�@�+@�
=@�ȴ@���@�ff@�M�@��@��-@�x�@�%@�  @�S�@�C�@�33@��!@�v�@�V@��@�@���@��h@�x�@�X@�/@��@�Ĝ@�Q�@�j@��
@��@���@��P@�l�@�K�@�+@�@��@���@�p�@��@��@�bN@��m@�dZ@�C�@��@��H@�n�@�-@�@��@���@��j@���@��D@�z�@�bN@��@�C�@�"�@��y@���@�v�@�v�@�$�@���@��7@��@��/@��D@�(�@�  @��
@��@���@�l�@�o@��@��H@���@�@���@���@���@�x�@��@��@�Ĝ@�Q�@�ƨ@�dZ@�;d@��@�I�@�-@��7@y%@r�\@k�
@`��@V@L�/@E�-@?+@9�7@5�@/l�@(A�@!�#@`B@��@~�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AݾwAݺ^Aݧ�A�ffA�\)A�&�A�ƨAܬAܧ�Aܥ�Aܣ�Aܡ�Aܝ�Aܝ�Aܛ�Aܙ�Aܕ�A܏\A܉7A܅A�|�A�x�A�r�A�n�A�jA�
=A�r�A�`BA�7LA���Aٛ�A�`BA�I�A�=qA�33A�%AؼjA�G�Aס�A�ZA�I�A�VA�XA�ĜA�+A�hsAҧ�A�ffA���AоwAϏ\A�x�A˥�A�Aǉ7A��HA�XA�jA�ĜA��A�M�A�&�A�-A��7A��A��A�^5A�A���A���A�C�A��RA���A�"�A�I�A��9A�A�E�A�ȴA�M�A��
A��A��wA��A��mA��7A���A�`BA�1'A�v�A���A��A~  AwS�Ap�An�Al�jAk7LAgx�Ad{Ab��A]|�AX�jAXZAV(�AS�hAR�AP��AM��AI��AG�TAF��ABQ�AA\)A>�HA;�wA:�A9XA5+A4r�A3��A2^5A0�A/oA.ffA,ĜA+dZA*^5A(��A&��A%ƨA$�DA$-A#��A#;dA"ȴA"A!?}A ��AoA �A/Az�AhsA|�A�HAA��A�TA�A�yAffAĜA|�AoA�!Az�A$�A��A�RA�A
=A
$�A�9A�^A;dAA�A�A�mA�AE�A�A��AE�AO�A �A Q�A =qA A�A r�A ffA �A33AG�A ~�A �A ĜA ��A �!A �DA bNA $�@���@�@��@�V@�+@�~�@��@���@���@�?}@�r�@� �@�1@��F@���@�@��/@���@�z�@�z�@�D@��u@�r�@�A�@�@��y@�-@��#@�h@�V@�z�@@�S�@��@��@�ff@�E�@��@�Q�@���@띲@�J@睲@�{@�G�@�V@�`B@�\@��@�x�@�7L@�j@�D@��@�x�@��@��`@�j@�9@�Z@�9X@�1'@�1@�S�@��H@�$�@�bN@۾w@���@�n�@��@��T@�x�@�1@֧�@�`B@�%@���@ԣ�@�j@�Q�@ӶF@ӍP@�|�@�t�@�t�@�;d@�ȴ@�5?@�@���@��@��@���@�`B@��@д9@�A�@�1@Ϯ@�C�@Ο�@���@�/@�Z@˾w@�+@��@�V@ɩ�@ə�@�/@��/@Ȭ@�Q�@�1@Ǯ@�\)@���@�ff@�J@Ų-@őh@���@��m@�"�@§�@��@���@���@�hs@��@�  @��F@�|�@�dZ@�+@��!@�^5@�$�@�hs@���@���@�bN@��F@�C�@�33@�
=@���@�n�@�V@�M�@�E�@�E�@�=q@��-@�(�@�K�@�ȴ@�=q@�{@��^@��D@��@��@���@��F@��P@�t�@�\)@��@�X@�V@��/@�1'@�\)@�"�@��+@�-@���@�`B@�?}@��@���@�9X@��m@��F@���@�l�@�@��!@��@�@��^@��7@�O�@�?}@�&�@�V@���@�Q�@���@�S�@�+@�
=@�ȴ@���@�ff@�M�@��@��-@�x�@�%@�  @�S�@�C�@�33@��!@�v�@�V@��@�@���@��h@�x�@�X@�/@��@�Ĝ@�Q�@�j@��
@��@���@��P@�l�@�K�@�+@�@��@���@�p�@��@��@�bN@��m@�dZ@�C�@��@��H@�n�@�-@�@��@���@��j@���@��D@�z�@�bN@��@�C�@�"�@��y@���@�v�@�v�@�$�@���@��7@��@��/@��D@�(�@�  @��
@��@���@�l�@�o@��@��H@���@�@���@���@���@�x�@��@��@�Ĝ@�Q�@�ƨ@�dZ@�;dG�O�@�I�@�-@��7@y%@r�\@k�
@`��@V@L�/@E�-@?+@9�7@5�@/l�@(A�@!�#@`B@��@~�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBbBhBhB\B
=B1B+B+B+BB  B
��B
��B
��BB%BDB�B#�B%�B0!BC�BC�B@�BF�BiyB}�By�Bu�Bs�Bw�Bs�Bu�B��B��B�BB �B�B\B
=B+B��B��B��B��B��B�bBu�BH�B�BDB
��B
�B
��B
ÖB
�FB
��B
�B
z�B
cTB
@�B
�B
  B	�5B	��B	��B	��B	o�B	aHB	T�B	I�B	8RB	$�B	�B	%B�B�B��B�B�B�yB�ZB�TB�HB�/B�B�
B�
B�B��B��B��B��B��B��B��B��BɺB��BȴBǮBĜB�wB�}BĜBɺB��B��BȴB��B��B��B�B��B��B��B��B��B��B��B��B��B�}B�qBBBBB��B��BBÖBÖBÖBŢBĜBBBBÖBÖBB��B��BÖBÖBĜBǮBɺB��B��B��B�#B�fB�B	%B	VB	hB	�B	&�B	/B	49B	:^B	<jB	<jB	=qB	@�B	?}B	A�B	N�B	P�B	P�B	Q�B	Q�B	R�B	XB	ZB	ZB	[#B	^5B	`BB	cTB	cTB	dZB	dZB	dZB	e`B	e`B	iyB	k�B	n�B	r�B	u�B	v�B	y�B	{�B	|�B	|�B	~�B	�B	�B	�PB	�VB	�VB	�hB	�uB	�oB	�VB	�7B	�7B	�=B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�9B	�FB	�LB	�RB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	B	B	B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�/B	�;B	�5B	�5B	�5B	�;B	�5B	�5B	�5B	�)B	�#B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�TB	�TB	�TB	�ZB	�`B	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
B
B
%B
%B
+B
+B
	7B
1B
hB
�B
$�B
-B
33B
7LB
;dB
B�B
E�B
K�B
P�B
S�B
YB
_;B
dZB
hsB
l�B
s�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�BB}B}BB}B}B}B{B}BtBvBsBkBjBhBkBeBeBYBFBLBJB>B
BBBBB�B
��B
��B
��B
��BB
B&B�B#�B%�B0BCyBCvB@fBF�Bi\B}�By�Bu�Bs�Bw�Bs�Bu�B��B̭B�oB�B �BlB8B
BB��B��B��B��B��B�<Bu�BH�B�B B
��B
�\B
��B
�rB
�%B
�fB
��B
z�B
c4B
@aB
�B	��B	�B	��B	�mB	�vB	o�B	a-B	T�B	I�B	87B	$�B	�B	B�B�B��B�B�B�aB�AB�>B�/B�B�B��B��B��B��B��B��B��B��BͺB̴B̵BɠB˭BțBǒBāB�]B�dBąBɞBˬBˮBȚBͷB��B��B��B��B��B��B��B��B��B��B̱B�gB�cB�VB�tB�sB�qB�rB�mB�lB�rB�zB�zB�zBņBĀB�sB�sB�sB�{B�wB�sB�eB�mB�yB�zBāBǏBɜB̲BͳB��B�B�GB�B	B	7B	FB	�B	&�B	.�B	4B	:<B	<GB	<HB	=NB	@_B	?[B	AfB	N�B	P�B	P�B	Q�B	Q�B	R�B	W�B	Y�B	Y�B	Z�B	^B	`B	c0B	c1B	d4B	d4B	d7B	e;B	e<B	iSB	kaB	ntB	r�B	u�B	v�B	y�B	{�B	|�B	|�B	~�B	��B	��B	�,B	�.B	�1B	�AB	�OB	�HB	�/B	�B	�B	�B	�5B	�~B	��B	��B	��B	��B	��B	�yB	�rB	�nB	�kB	�lB	�oB	�lB	�cB	�eB	�aB	�UB	�LB	�HB	�WB	�aB	�yB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�$B	�*B	�)B	�0B	�2B	�;B	�GB	�QB	�TB	�TB	�TB	�TB	�[B	�[B	�aB	�fB	�hB	�fB	�gB	�mB	�tB	�qB	�rB	�rB	�uB	�tB	�sB	�xB	ǅB	ȌB	ɓB	̥B	ήB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�
B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�+B	�'B	�/B	�5B	�6B	�CB	�AB	�CB	�KB	�GB	�HB	�IB	�NB	�OB	�ZB	�aB	�lB	�nB	�rB	�sB	�rB	�wB	�xB	�wB	�yB	�zB	�zB	�|B	�B	�B	�B	�B	�~B	�B	�{B	�qB	�rB	�pB	�nB	�lB	�hB	�dB	�fB	�tB	�yB	�wB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
G�O�B
B
:B
}B
$�B
,�B
3	B
7B
;6B
BaB
EtB
K�B
P�B
S�B
X�B
_B
d*B
hEB
l]B
s�B
v�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451102016080714511020160807145110  AO  ARCAADJP                                                                    20150226221408    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221408  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221408  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145110  IP                  G�O�G�O�G�O�                