CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:34Z AOML 3.0 creation; 2016-05-31T19:14:33Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230534  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               6A   AO  4051_7090_054                   2C  D   APEX                            5368                            041511                          846 @ֻ�$h�	1   @ֻѳ3�@5 A�7K��e(bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    6A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BQ33BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyl�D��D�@ D���D��3D���D�)�D�s3D��fD��D�FfD���D��fD��D�6fD�l�D���D�fD�9�D�vfD�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A�A;�A[�A{�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBP{BVz�B^�HBf�HBn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
nD
�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D nD �D!nD!�D"nD"�D#nD#�D$nD$�D%nD%�D&nD&�D'nD'�D(nD(�D)nD)�D*nD*�D+nD+�D,nD,�D-nD-�D.nD.�D/nD/�D0nD0�D1nD1�D2nD2�D3nD3�D4nD4�D5nD5�D6nD6�D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�D=nD=�D>nD>�D?nD?�D@nD@�DAnDA�DBnDB�DCnDC�DDnDD�DEnDE�DFnDF�DGnDG�DHnDH�DInDI�DJnDJ�DKnDK�DLnDL�DMnDM�DNnDN�DOnDO�DPnDP�DQnDQ�DRnDR�DSnDS�DTnDT�DUnDU�DVnDV�DWnDW�DXnDX�DYnDY�DZnDZ�D[nD[�D\nD\�D]nD]�D^nD^�D_nD_�D`nD`�DanDa�DbnDb�DcnDc�DdnDd�DenDe�DfnDf�DgnDg�DhnDh�DinDi�DjnDj�DknDk�DlnDl�DmnDm�DnnDn�DonDo�DpnDp�DqnDq�DrnDr�DsnDs�DtTzDyZ�D��D�7
D���D��=D��D� �D�j=D��pD��D�=pD���D��pD��D�-pD�c�D���D��pD�0�D�mpD�`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��`A��`A��`A��mA��HA��;Aܛ�Aܗ�Aܣ�Aܝ�A܇+A�?}A��A�1A�1A�A�oA�&�A��A�  A�1AۑhA��HA�dZA�Q�A� �Aˣ�A�ffA��mA���A�l�A��A�I�A�A���A��jA��#A���A�S�A�ZA��A���A�%A���A�ƨA�t�A���A��A�ȴA��A��!A�|�A�ȴA�?}A�S�A�^5A�ȴA��A�\)A�I�A��RA�1'A��A��A���A�VA�|�A�=qA���A���A�K�A� �A�x�A�bA�  A��mA��hA��A��A�ffA�5?A�9XA�&�A�I�A���A�S�A���A��A�JA�
=A�^5A��A�^5A��9A�  A�t�A���A�?}A�/A�&�A��/A�r�A�^A|��Ay�Av5?Asp�Ao��Al��Aj�/Ai�Af�HAdffAa;dA^�A\�9AZffAX�uAWVAS��ASS�AQ�FAO�AK��AJ�yAJ�jAJffAI�#AH�jAH  AG��AG"�AF�+AE
=AC��AC7LAB$�AB  AA��A@��A?�A>r�A>1'A=�A<n�A;S�A:�+A9?}A8v�A5�A2ȴA/ƨA.��A-�FA,v�A+�
A*��A)C�A'��A%S�A$Q�A"�A n�A&�A�#Az�AG�A��A�AXA9XA|�AĜA�-A��A=qA�TA��A7LA��A1'A��A"�A�FA
�RA
ZA
1A	x�A�A��A~�AE�Ax�AE�A;dAA��A�uA^5AM�AQ�AE�A �A  A��AhsA��AQ�A  A�AƨA+A �@��
@���@��-@��@��@�K�@�5?@�|�@�bN@�P@���@�{@�%@�@�ff@�7L@�@�-@�7@�Q�@��m@�33@�$�@�O�@�w@�M�@�j@���@�33@�V@��@���@� �@֟�@�@�G�@��@Ԭ@�ƨ@���@��@Гu@��@�C�@̼j@� �@�dZ@�;d@�
=@��y@�ȴ@ʏ\@��T@�X@ȃ@ǶF@Ǖ�@�\)@��@Ƈ+@�J@��T@��T@��T@���@š�@�O�@�z�@�l�@�=q@�V@�I�@�l�@���@���@�O�@�V@���@��@���@��\@�v�@�$�@��7@���@�b@��@��\@���@�hs@��`@���@���@��@��u@���@��@�^5@��@�"�@��7@��@��@��P@�+@�
=@��@���@��T@��#@���@��@�r�@���@��@��!@�~�@�M�@��@���@��`@�Z@�1@��P@��@��!@�-@�G�@�%@�Ĝ@��@�A�@��;@��@�S�@�o@�
=@�@��@��y@��@��R@��\@�v�@�V@�5?@�$�@��@�{@�{@�J@���@��@���@��-@���@�x�@�O�@�&�@���@�bN@��@��@�$�@���@��#@���@�p�@�X@�O�@�O�@�O�@�G�@�O�@�O�@�X@�X@�X@�X@�O�@�G�@�?}@�/@�/@�&�@�V@���@���@��D@��m@�dZ@�C�@�C�@�C�@�C�@�
=@���@���@���@��@��y@��H@���@��R@��R@���@�v�@�J@���@�Q�@�I�@�A�@�1'@� �@�1@��m@�ƨ@��F@��P@�|�@�dZ@���@��+@�~�@�v�@�n�@�M�@�-@�$�@��@���@��-@���@�hs@�G�@��@�%@��j@���@�9X@��m@��w@�l�@��y@��\@�-@��T@��^@���@��@�p�@�O�@���@��@�9X@�  @��;@��w@���@�t�@�dZ@�;d@�o@���@��@��H@�^5@�@��h@�hs@�G�@�7L@�&�@���@���@x  @o��@h�u@d��@`�u@Y�@P�@J~�@D�j@>v�@6��@-��@"-@�+@�
@��@z�@@1'@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��`A��`A��`A��mA��HA��;Aܛ�Aܗ�Aܣ�Aܝ�A܇+A�?}A��A�1A�1A�A�oA�&�A��A�  A�1AۑhA��HA�dZA�Q�A� �Aˣ�A�ffA��mA���A�l�A��A�I�A�A���A��jA��#A���A�S�A�ZA��A���A�%A���A�ƨA�t�A���A��A�ȴA��A��!A�|�A�ȴA�?}A�S�A�^5A�ȴA��A�\)A�I�A��RA�1'A��A��A���A�VA�|�A�=qA���A���A�K�A� �A�x�A�bA�  A��mA��hA��A��A�ffA�5?A�9XA�&�A�I�A���A�S�A���A��A�JA�
=A�^5A��A�^5A��9A�  A�t�A���A�?}A�/A�&�A��/A�r�A�^A|��Ay�Av5?Asp�Ao��Al��Aj�/Ai�Af�HAdffAa;dA^�A\�9AZffAX�uAWVAS��ASS�AQ�FAO�AK��AJ�yAJ�jAJffAI�#AH�jAH  AG��AG"�AF�+AE
=AC��AC7LAB$�AB  AA��A@��A?�A>r�A>1'A=�A<n�A;S�A:�+A9?}A8v�A5�A2ȴA/ƨA.��A-�FA,v�A+�
A*��A)C�A'��A%S�A$Q�A"�A n�A&�A�#Az�AG�A��A�AXA9XA|�AĜA�-A��A=qA�TA��A7LA��A1'A��A"�A�FA
�RA
ZA
1A	x�A�A��A~�AE�Ax�AE�A;dAA��A�uA^5AM�AQ�AE�A �A  A��AhsA��AQ�A  A�AƨA+A �@��
@���@��-@��@��@�K�@�5?@�|�@�bN@�P@���@�{@�%@�@�ff@�7L@�@�-@�7@�Q�@��m@�33@�$�@�O�@�w@�M�@�j@���@�33@�V@��@���@� �@֟�@�@�G�@��@Ԭ@�ƨ@���@��@Гu@��@�C�@̼j@� �@�dZ@�;d@�
=@��y@�ȴ@ʏ\@��T@�X@ȃ@ǶF@Ǖ�@�\)@��@Ƈ+@�J@��T@��T@��T@���@š�@�O�@�z�@�l�@�=q@�V@�I�@�l�@���@���@�O�@�V@���@��@���@��\@�v�@�$�@��7@���@�b@��@��\@���@�hs@��`@���@���@��@��u@���@��@�^5@��@�"�@��7@��@��@��P@�+@�
=@��@���@��T@��#@���@��@�r�@���@��@��!@�~�@�M�@��@���@��`@�Z@�1@��P@��@��!@�-@�G�@�%@�Ĝ@��@�A�@��;@��@�S�@�o@�
=@�@��@��y@��@��R@��\@�v�@�V@�5?@�$�@��@�{@�{@�J@���@��@���@��-@���@�x�@�O�@�&�@���@�bN@��@��@�$�@���@��#@���@�p�@�X@�O�@�O�@�O�@�G�@�O�@�O�@�X@�X@�X@�X@�O�@�G�@�?}@�/@�/@�&�@�V@���@���@��D@��m@�dZ@�C�@�C�@�C�@�C�@�
=@���@���@���@��@��y@��H@���@��R@��R@���@�v�@�J@���@�Q�@�I�@�A�@�1'@� �@�1@��m@�ƨ@��F@��P@�|�@�dZ@���@��+@�~�@�v�@�n�@�M�@�-@�$�@��@���@��-@���@�hs@�G�@��@�%@��j@���@�9X@��m@��w@�l�@��y@��\@�-@��T@��^@���@��@�p�@�O�@���@��@�9X@�  @��;@��w@���@�t�@�dZ@�;d@�o@���@��@��H@�^5@�@��h@�hs@�G�@�7L@�&�@���@���@x  @o��@h�u@d��@`�u@Y�@P�@J~�@D�j@>v�@6��@-��@"-@�+@�
@��@z�@@1'@I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�yB�yB�/B�)B�/B�)B�B��B��B��B��B��B��B�B�B�B�5B��B��B��Bo�B�1B�PB�PB�+B�B|�By�Bv�Bt�Bo�B`BBO�BH�BI�BK�BH�BE�BL�BZBYBD�B5?B2-B1'B2-B/B#�B�B�B\BVBPB
=B��B�B�B��B��BȴB�XB��B�oB�VB�7B�B�B� Bz�B�B�%B�=B�By�B[#B=qB$�B"�B�B
=B�B�qB��B�hB�Bt�BiyBcTBVBH�B;dB.B
��B
�wB
�B
bNB
S�B
M�B
9XB
#�B
PB	��B	�sB	�B	��B	��B	�FB	��B	��B	�oB	�+B	}�B	r�B	k�B	cTB	YB	S�B	L�B	A�B	6FB	2-B	1'B	/B	,B	'�B	#�B	 �B	�B	�B	�B	hB	JB	%B	B	  B��B��B�B�B�B�yB�`B�NB�5B�B��BǮB�}B�jB�RB�?B�3B�B��B��B��B��B��B�uB�\B�DB�+B�B� Bz�Bw�Bu�Bt�Br�Bp�Bp�Bo�Bn�Bm�Bl�Bk�BjBiyBiyBhsBgmBffBe`Be`Be`Be`BdZBcTBbNBaHBaHBaHBaHBaHBaHBaHB`BB`BB`BB_;B_;B_;B^5B]/B]/B]/B\)B[#B[#BZBYBYBYBYBXBVBVBYBYBZBZBZB[#B[#B[#B]/B]/B]/B^5B_;B_;BaHBcTBffBgmBiyBjBjBk�Bl�Bm�Bn�Bq�Br�Bt�Bu�Bu�Bv�Bz�B|�B|�B}�B}�B�B�%B�PB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�RB�^B�qBBǮBȴBɺB��B��B��B��B��B��B�B�B�#B�5B�BB�NB�TB�ZB�mB�B�B�B�B��B��B��B��B	B	%B	+B	
=B	JB	PB	PB	\B	uB	uB	{B	�B	�B	�B	#�B	'�B	)�B	+B	,B	.B	33B	6FB	8RB	<jB	@�B	B�B	F�B	O�B	Q�B	T�B	YB	]/B	cTB	ffB	k�B	p�B	r�B	s�B	t�B	u�B	u�B	w�B	y�B	z�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�LB	�RB	�XB	�XB	�XB	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�BB	�NB	�TB	�TB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
#�B
(�B
.B
5?B
9XB
@�B
A�B
F�B
J�B
R�B
^5B
aHB
cTB
ffB
k�B
l�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�NB�FB�NB�FB�8B��B��B��B��B��B�B�?B�5B�/B�QB�B��B��Bo�B�NB�kB�nB�FB�+B}	By�Bv�Bt�Bo�B`^BO�BH�BI�BK�BH�BE�BL�BZ=BY2BD�B5\B2KB1CB2GB/4B#�B�B�BwBqBhB
YB�B�B�8B�B��B��B�tB��B��B�pB�PB�9B�7B�Bz�B�4B�AB�UB�>By�B[@B=�B$�B"�B�B
XB�B��B�B��B�6Bt�Bi�BcpBV"BH�B;�B.0B
�
B
��B
�=B
brB
TB
M�B
9{B
#�B
rB	��B	�B	�<B	��B	��B	�mB	�B	��B	��B	�SB	~B	r�B	k�B	c�B	YEB	T&B	L�B	A�B	6rB	2ZB	1SB	/IB	,5B	(B	$B	 �B	�B	�B	�B	�B	xB	VB	JB	 /B�	B��B��B��B�B�B�B�B�gB�NB�B��B��B��B��B�rB�gB�PB�0B�B��B��B��B��B��B�zB�bB�PB�9B{BxBu�Bt�Br�Bp�Bp�Bo�Bn�Bm�Bl�Bk�Bj�Bi�Bi�Bh�Bg�Bf�Be�Be�Be�Be�Bd�Bc�Bb�Ba�Ba�Ba�Ba�Ba�Ba�Ba�B`{B`{B`{B_uB_uB_sB^mB]eB]iB]gB\bB[]B[]BZVBYQBYOBYPBYOBXJBV@BV;BYOBYSBZVBZSBZVB[]B[]B[\B]fB]hB]kB^pB_uB_uBa�Bc�Bf�Bg�Bi�Bj�Bj�Bk�Bl�Bm�Bn�Bq�Br�Bt�Bu�Bu�BwB{B}%B}'B~*B~,B�MB�]B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�"B�&B�+B�,B�,B�3B�3B�LB�YB�cB��B��B��B��B��B��B��B��B�B� B�(B�'B�-B�;B�DB�WB�kB�vB�B�B�B�B��B��B��B��B�B�B�(B�-B	<B	WB	]B	
qB	|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$B	(!B	*/B	+4B	,;B	.DB	3eB	6yB	8�B	<�B	@�B	B�B	F�B	PB	RB	U-B	YHB	]aB	c�B	f�B	k�B	p�B	r�B	s�B	t�B	u�B	u�B	w�B	z	B	{B	} B	)B	�-B	�-B	�6B	�5B	�5B	�3B	�?B	�<B	�AB	�JB	�SB	�^B	�mB	�yB	��B	��B	��B	��B	��B	� B	�	B	�B	�B	�B	�B	� B	�B	� B	� B	� B	� B	�B	�%B	�&B	�&B	�&B	�!B	�$B	�"B	�)B	�*B	�.B	�=B	�YB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�!B	�#B	�+B	�7B	�=B	�<B	�=B	�=B	�;B	�<B	�<B	�HB	�JB	�JB	�IB	�KB	�OB	�TB	�SB	�bB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B
1B
�B
�B
$B
)#B
.?B
5gB
9�B
@�B
A�B
F�B
J�B
SB
^^B
apB
c|B
f�B
k�B
l�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.28 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230534  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230534  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                