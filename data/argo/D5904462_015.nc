CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:15Z AOML 3.0 creation; 2016-08-07T21:51:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221415  20160807145111  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_015                   2C  D   APEX                            6529                            072314                          846 @���?�1   @�󪪿�@2�$�/�c��n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  BffB��B ffB(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD���D�P D���D�� D�3D�I�D��fD��3D�fD�I�D�p D��3D�fD�I�Dڌ�D�fD��D�I�D�3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A$��AD��Ad��A�Q�A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B�\BB!�\B)(�B1(�B9(�BA�\BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB��{B�aHB��{B��{BĔ{BȔ{B̔{B�ǮB�aHB�aHBܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4c�C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�)D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtx�Dy��D�D�YHD���D��HD�{D�R�D���D��{D��D�R�D�yHD��{D��D�R�DږD࿮D�D�R�D�{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AڶFAڴ9Aڴ9Aڴ9AڶFAڶFAڶFAڸRAڶFAڶFAڶFAڸRAڸRAڸRAڸRAں^Aں^Aں^Aں^AڼjAڼjAڼjAھwA���AھwAڍPA���A���Aٰ!Aٙ�A�r�A�l�A�G�A�$�A�1A��AؓuA�n�A��/A��A�&�A�z�A�O�A�VAӁA��AжFAΙ�A�dZA�XAɅA��Aț�A��/A�/A�|�A��A�n�A�M�A��^A�{A���A�`BA���A�jA��A��DA�%A��uA���A�VA��yA�;dA�;dA���A�oA���A�?}A��uA� �A��FA�(�A�G�A�A���A�O�A�x�A��A���A���A��9A���A�A��!A�%A���A���A�-A�=qA�A���A�t�A��RA��A�A���A���A��PA�1A�r�A��`A�A�A��#A{�hAw/Ar��AqK�ApAm
=Ak�AkAd��A^�9A[�PAY��AX��AWO�AU�wATffAP�DAN�AK��AJ�!AH�+AE�
AB�yA@9XA>A�A<�A;��A:�DA8�A6ĜA5�A6 �A6(�A4�HA3�#A3��A3�A1VA0$�A.�A-��A,�A)C�A& �A#p�A"^5A"n�A!ƨA �AC�A��AC�At�A�hA~�AXA�\AM�Ax�AQ�A5?AE�A�AA��A��A��Ax�AVA��AĜA�DAZAE�A=qA(�A��A�
AK�A
M�A
$�A
{A	��A	O�A	p�A��A33AffA�#AG�Ar�A5?A��AƨAƨA��A��A�9A�A�PAC�A&�A ��A �RA �A b@�l�@��@��\@��u@��m@�C�@��H@��+@�V@�O�@��/@��@�|�@���@�^5@���@���@�bN@��m@�C�@��@�^5@���@�G�@��D@�9@�O�@�/@�z�@�"�@�n�@�E�@�V@�n�@��#@�`B@�@�33@�"�@���@��@�
=@�C�@���@���@�u@�w@�F@�|�@��@��@�33@�M�@�^5@�x�@݁@݉7@�`B@���@�1'@�C�@���@��#@ج@�ƨ@֟�@�G�@��`@ԣ�@�A�@�ƨ@Ӆ@�33@��@ҏ\@�E�@���@Ѻ^@У�@�1'@��
@�ȴ@���@Ͳ-@�V@��@̬@�z�@�1'@ˍP@�C�@�o@�ff@�5?@���@�@ɩ�@�/@�%@�Ĝ@�z�@��@���@ǶF@�dZ@Ƈ+@�=q@�@��@��T@�G�@�z�@���@�|�@�"�@°!@�~�@�V@�$�@���@��h@��@�Q�@�b@��;@�ƨ@�;d@��@�b@�|�@�33@��@�$�@��7@�O�@�G�@���@�Z@�ƨ@�S�@��@�V@�^5@�=q@�5?@�=q@��^@�&�@��w@���@��+@�@�{@�{@��@��h@���@�Z@�  @��
@��w@���@�o@��@�p�@���@��m@�ƨ@��w@�o@��@�hs@���@��u@��
@�33@��!@�=q@��@��7@�G�@��@��u@��
@�C�@���@�M�@���@��@��/@���@�j@�I�@�(�@�  @���@�|�@�;d@�+@���@�v�@�-@��#@���@�O�@��9@��;@�t�@�+@���@�~�@�=q@�@��@��^@��@�`B@�7L@��@�V@��@��/@�(�@��;@���@�|�@�\)@�;d@�
=@�ȴ@�n�@��@���@���@�O�@��`@�j@�Z@�I�@�1'@�(�@��@���@���@�K�@��y@���@��@��^@��7@�7L@���@���@��9@�A�@�  @��
@���@�S�@�"�@��@���@�ȴ@��!@�n�@�=q@�{@��#@��-@��H@��+@�1'@�bN@u�@k@b�H@[�
@R�@J��@C�
@<��@7|�@1hs@+"�@%O�@!%@(�@�@��@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AڶFAڴ9Aڴ9Aڴ9AڶFAڶFAڶFAڸRAڶFAڶFAڶFAڸRAڸRAڸRAڸRAں^Aں^Aں^Aں^AڼjAڼjAڼjAھwA���AھwAڍPA���A���Aٰ!Aٙ�A�r�A�l�A�G�A�$�A�1A��AؓuA�n�A��/A��A�&�A�z�A�O�A�VAӁA��AжFAΙ�A�dZA�XAɅA��Aț�A��/A�/A�|�A��A�n�A�M�A��^A�{A���A�`BA���A�jA��A��DA�%A��uA���A�VA��yA�;dA�;dA���A�oA���A�?}A��uA� �A��FA�(�A�G�A�A���A�O�A�x�A��A���A���A��9A���A�A��!A�%A���A���A�-A�=qA�A���A�t�A��RA��A�A���A���A��PA�1A�r�A��`A�A�A��#A{�hAw/Ar��AqK�ApAm
=Ak�AkAd��A^�9A[�PAY��AX��AWO�AU�wATffAP�DAN�AK��AJ�!AH�+AE�
AB�yA@9XA>A�A<�A;��A:�DA8�A6ĜA5�A6 �A6(�A4�HA3�#A3��A3�A1VA0$�A.�A-��A,�A)C�A& �A#p�A"^5A"n�A!ƨA �AC�A��AC�At�A�hA~�AXA�\AM�Ax�AQ�A5?AE�A�AA��A��A��Ax�AVA��AĜA�DAZAE�A=qA(�A��A�
AK�A
M�A
$�A
{A	��A	O�A	p�A��A33AffA�#AG�Ar�A5?A��AƨAƨA��A��A�9A�A�PAC�A&�A ��A �RA �A b@�l�@��@��\@��u@��m@�C�@��H@��+@�V@�O�@��/@��@�|�@���@�^5@���@���@�bN@��m@�C�@��@�^5@���@�G�@��D@�9@�O�@�/@�z�@�"�@�n�@�E�@�V@�n�@��#@�`B@�@�33@�"�@���@��@�
=@�C�@���@���@�u@�w@�F@�|�@��@��@�33@�M�@�^5@�x�@݁@݉7@�`B@���@�1'@�C�@���@��#@ج@�ƨ@֟�@�G�@��`@ԣ�@�A�@�ƨ@Ӆ@�33@��@ҏ\@�E�@���@Ѻ^@У�@�1'@��
@�ȴ@���@Ͳ-@�V@��@̬@�z�@�1'@ˍP@�C�@�o@�ff@�5?@���@�@ɩ�@�/@�%@�Ĝ@�z�@��@���@ǶF@�dZ@Ƈ+@�=q@�@��@��T@�G�@�z�@���@�|�@�"�@°!@�~�@�V@�$�@���@��h@��@�Q�@�b@��;@�ƨ@�;d@��@�b@�|�@�33@��@�$�@��7@�O�@�G�@���@�Z@�ƨ@�S�@��@�V@�^5@�=q@�5?@�=q@��^@�&�@��w@���@��+@�@�{@�{@��@��h@���@�Z@�  @��
@��w@���@�o@��@�p�@���@��m@�ƨ@��w@�o@��@�hs@���@��u@��
@�33@��!@�=q@��@��7@�G�@��@��u@��
@�C�@���@�M�@���@��@��/@���@�j@�I�@�(�@�  @���@�|�@�;d@�+@���@�v�@�-@��#@���@�O�@��9@��;@�t�@�+@���@�~�@�=q@�@��@��^@��@�`B@�7L@��@�V@��@��/@�(�@��;@���@�|�@�\)@�;d@�
=@�ȴ@�n�@��@���@���@�O�@��`@�j@�Z@�I�@�1'@�(�@��@���@���@�K�@��y@���@��@��^@��7@�7L@���@���@��9@�A�@�  @��
@���@�S�@�"�@��@���@�ȴ@��!@�n�@�=q@�{@��#G�O�@��H@��+@�1'@�bN@u�@k@b�H@[�
@R�@J��@C�
@<��@7|�@1hs@+"�@%O�@!%@(�@�@��@\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ǮB
��B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��BB	7B�B�B%�B'�B+B/B;dBE�BYBffB�=B�9B��B��B�ZBB#�BC�BjB�B�?B��B�{B�JBw�BI�B9XB1'B2-B2-B33B33B33B33B33B2-B,B�BPB%B��B��B1BhB33B;dBE�BO�BN�BI�B7LB�`BĜBƨBƨB�}B�3B�9B�B�PBs�BbNBQ�B1'B�B	7B
��B
�mB
�
B
�^B
��B
r�B
G�B
\B	�HB	�jB	��B	�bB	�%B	r�B	hsB	`BB	?}B	 �B	oB	DB	+B	B��B�B�yB�`B�fB�ZB�ZB�B�B�B��B��B��B��B��B��B	B	{B	.B	1'B	>wB	E�B	G�B	K�B	J�B	E�B	@�B	8RB	 �B	bB	1B	PB	{B	�B	\B	\B	oB	bB	B	B	B	%B	%B	%B	B��B�B�B�B�B��B��B��B	1B	PB	bB	oB	oB	oB	oB	hB	bB	JB		7B	+B	B	B	B	B	�B	"�B	%�B	%�B	%�B	'�B	+B	6FB	<jB	D�B	E�B	E�B	F�B	K�B	Q�B	R�B	VB	W
B	T�B	VB	XB	[#B	_;B	bNB	cTB	ffB	m�B	o�B	p�B	p�B	p�B	q�B	r�B	q�B	r�B	r�B	q�B	p�B	r�B	s�B	u�B	w�B	x�B	z�B	{�B	}�B	�B	�B	�7B	��B	��B	��B	��B	��B	��B	��B	�\B	�PB	�JB	�=B	�%B	�7B	�hB	�oB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�?B	�XB	�jB	�qB	�}B	�}B	��B	ÖB	ŢB	ŢB	ŢB	ĜB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�#B	�#B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�#B	�B	�B	�B	�B	�
B	�
B	�B	�B	��B	�
B	�
B	�B	��B	��B	��B	��B	�/B	�;B	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�TB	�HB	�TB	�TB	�NB	�`B	�`B	�ZB	�NB	�HB	�NB	�TB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
{B
oB
�B
"�B
,B
33B
8RB
@�B
@�B
J�B
N�B
R�B
YB
_;B
cTB
gmB
jB
n�B
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
ȡB
ȡB
ȡB
ȡB
ȡB
ȡB
ȡB
ȡB
ȡB
ǚB
ȣB
ȡB
ȡB
ǚB
ǜB
ǚB
ǚB
ȡB
ȡB
ȡB
ȡB
ȣB
ȟB
ǛB
ǚB
��B
�wB
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B�B	"BkB�B%�B'�B*�B/B;OBE�BX�BfOB�(B�!B�mB̶B�BB �B#�BCBjfB��B�'B��B�dB�0Bw�BI�B9@B1B2B2B3B3B3B3B3B2B+�B~B5BB��B��BBOB3B;KBE�BO�BN�BI�B7.B�EBăBƋBƌB�dB�B�B��B�3Bs�Bb2BQ�B1B�B	B
��B
�SB
��B
�DB
��B
r�B
G�B
EB	�6B	�YB	��B	�PB	�B	r�B	haB	`1B	?nB	 �B	aB	5B	B	 �B��B�B�mB�RB�VB�MB�NB�qB�B�B��B��B��B��B��B��B		B	lB	.B	1B	>dB	E�B	G�B	K�B	J�B	E�B	@nB	8?B	 �B	NB	B	>B	jB	oB	JB	HB	\B	QB	B	 �B	B	B	B	B	�B��B�B�sB�B�B��B��B��B	 B	<B	PB	ZB	^B	[B	ZB	TB	NB	7B		"B	B	B	�B	B	B	�B	"�B	%�B	%�B	%�B	'�B	*�B	6.B	<VB	D�B	E�B	E�B	F�B	K�B	Q�B	R�B	U�B	V�B	T�B	U�B	W�B	[
B	_$B	b8B	c>B	fLB	m{B	o�B	p�B	p�B	p�B	q�B	r�B	q�B	r�B	r�B	q�B	p�B	r�B	s�B	u�B	w�B	x�B	z�B	{�B	}�B	��B	��B	�B	�rB	��B	��B	��B	��B	��B	�sB	�EB	�6B	�/B	�#B	�B	�B	�PB	�UB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�OB	�UB	�aB	�aB	�hB	�{B	ňB	ŅB	ņB	ĀB	ƌB	ƌB	ǔB	ɠB	ɠB	ʥB	˩B	˪B	͸B	ξB	λB	μB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	� B	� B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�*B	�1B	�5B	�7B	�8B	�6B	�7B	�8B	�;B	�6B	�)B	�7B	�7B	�/B	�DB	�DB	�>B	�/B	�)B	�/B	�7B	�PB	�\B	�ZB	�ZB	�aB	�aB	�bB	�`B	�_B	�`B	�cB	�`B	�_B	�_B	�hB	�iB	�gB	�gB	�fB	�iB	�hB	�fB	�gB	�gB	�bB	�^B	�`B	�gB	�gB	�iB	�fB	�nB	�nB	�tB	�lB	�mB	�mB	�sB	�rB	�yB	�B	�yB	�xB	�vB	�~B	�B	�zB	�zB	�yB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
B
[B
NB
�B
"�B
+�B
3B
82B
@bB
@aB
J�B
N�B
R�B
X�B
_B
c0B
gKB
j\B
ntB
r�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451112016080714511120160807145111  AO  ARCAADJP                                                                    20150226221415    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221415  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221415  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145111  IP                  G�O�G�O�G�O�                