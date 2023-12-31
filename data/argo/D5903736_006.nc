CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:09Z AOML 3.0 creation; 2016-05-31T19:14:25Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230509  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_006                   2C  D   APEX                            5368                            041511                          846 @�@�VH 1   @�@��Р
@3��-�dG�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�	�D�C3D��fD�ɚD� D�I�D�vfD��3D��D�@ D��3D���D�3D�FfD�VfD�ٚD�  D�L�D�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@�Q�A�\A:�\A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�RB�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1�)C3�)C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dw
D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'p�D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�D.p�D.�D/p�D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4p�D4�D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�DRp�DR�DSp�DS�DTp�DT�DUp�DU�DVp�DV�DWp�DW�DXp�DX�DYp�DY�DZp�DZ�D[p�D[�D\p�D\�D]p�D]�D^p�D^�D_p�D_�D`p�D`�Dap�Da�Dbp�Db�Dcp�Dc�Ddp�Dd�Dep�De�Dfp�Df�Dgp�Dg�Dhp�Dh�Dip�Di�Djp�Dj�Dkp�Dk�Dlp�Dl�Dmp�Dm�Dnp�Dn�Dop�Do�Dpp�Dp�Dqp�Dq�Drp�Dr�Dsp�Ds�Dtj>Dy�
D��D�;�D�~�D���D�RD�A�D�n�D�˅D�D�8RD�{�D��D���D�>�D�N�D���D��RD�ED�{�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�AˋDA˕�AˑhA˛�A˟�A˟�A���A���A�A�VA�bA��A��A��A��`A��A���A��TA��
A��#A���A���A˺^A˙�A�K�A�bA��yAʾwAʬAʬAʧ�AʼjA�ȴAʸRAʮAʍPA�S�A��TA�VA�$�A���Aș�A���A�;dA���A�M�A���A��HAŰ!A�K�AąA��A���A�5?A��A���A���A��A��A�1A�~�A�5?A�9XA�ZA�S�A�"�A�|�A��A�
=A��HA�?}A�I�A���A�ffA��wA�S�A��/A���A�dZA��A�/A�ĜA�|�A�ffA�`BA� �A�^5A��jA��A��#A�K�A�Q�A�9XA�  A�ĜA�  A��TA��hA�ĜA�VA�t�A���A���A�I�A�ȴA���A�I�A�z�A���A�?}A���A���A�n�A�jA���A��
A���A�ZA�%A�VA&�AzȴAyVAw�AtArjAp�An�9Ak�;Ahv�Af�DAcC�Aa�TA`ffA_�PA]�AZ�yAX�AW%AVbNAU�FAT�RAT�ASVAQt�AP�`AP�/AP�AO��AN�AL�AK�#AK�7AJ  AH�AF^5AE�
AEO�AD�jAD�AA�PA@�A?��A>�!A=��A;�^A9��A89XA7A6z�A6 �A4�A3�A3�A2M�A1?}A/|�A/C�A.=qA+"�A)��A(  A&{A%S�A#�mA!|�A A�At�A�\A��A��A/A�HAv�AVA��AȴA  A`BA�A��At�A�9A^5A  A��A(�A��A�AȴA~�AffA�A��AffA��A;dA
I�A
5?A	�A	+A9XA�HA7LA�A�uAJAl�AffA Ĝ@�dZ@�j@�J@�ff@���@�S�@�~�@���@���@���@�7@��`@��m@��y@�&�@��@�Z@�Z@�ƨ@��@��@��@�hs@��/@��T@�+@�`B@� �@�@ڟ�@�n�@�v�@�^5@�{@�Q�@�@֗�@�-@���@ա�@�x�@�1@�@�V@�(�@ϝ�@�C�@�
=@���@��T@��@�Z@˥�@�S�@�K�@���@�@��@�  @�V@���@Ĵ9@�bN@� �@�1@�l�@��@�7L@��m@��@�@�x�@�7L@�j@�ƨ@�C�@�$�@��@��T@�@�-@�V@�V@���@��H@��#@�{@�5?@�ff@���@�|�@���@�v�@��7@���@�Q�@�A�@��m@�|�@�;d@���@�5?@��@���@�X@�V@���@��u@�I�@��P@�|�@�C�@��H@�5?@�@�/@���@��@���@�r�@�bN@�  @���@���@�+@���@���@�ff@�{@��@��@���@�%@���@���@���@�9X@�1@��m@��F@�|�@�S�@�K�@���@��F@��@�K�@��@���@�J@��-@��7@�x�@�`B@�&�@��/@���@�bN@��@���@�C�@�+@��@���@��\@�$�@���@�p�@�p�@�E�@�@�V@�Ĝ@���@� �@�b@�  @��@��
@���@���@��P@�dZ@�@���@��\@�{@���@�G�@�V@���@��/@�V@��@��
@���@��y@��R@���@���@��\@�@��7@�X@�7L@�V@�7L@�/@���@�Ĝ@���@�j@� �@�b@�ƨ@�dZ@�S�@�+@�"�@�;d@�+@�$�@�`B@�%@��D@�r�@�bN@�(�@��w@��P@�|�@�K�@�;d@�o@���@�M�@�n�@�v�@�^5@�@��^@���@��7@��@�hs@�/@��@��D@�(�@��F@�C�@�@�ȴ@�n�@��h@�G�@��@�;d@��@{�m@tj@l�@e��@^�@W+@O��@G��@A7L@:�H@4�j@/��@)��@$��@   @S�@��@�!@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�z�AˋDA˕�AˑhA˛�A˟�A˟�A���A���A�A�VA�bA��A��A��A��`A��A���A��TA��
A��#A���A���A˺^A˙�A�K�A�bA��yAʾwAʬAʬAʧ�AʼjA�ȴAʸRAʮAʍPA�S�A��TA�VA�$�A���Aș�A���A�;dA���A�M�A���A��HAŰ!A�K�AąA��A���A�5?A��A���A���A��A��A�1A�~�A�5?A�9XA�ZA�S�A�"�A�|�A��A�
=A��HA�?}A�I�A���A�ffA��wA�S�A��/A���A�dZA��A�/A�ĜA�|�A�ffA�`BA� �A�^5A��jA��A��#A�K�A�Q�A�9XA�  A�ĜA�  A��TA��hA�ĜA�VA�t�A���A���A�I�A�ȴA���A�I�A�z�A���A�?}A���A���A�n�A�jA���A��
A���A�ZA�%A�VA&�AzȴAyVAw�AtArjAp�An�9Ak�;Ahv�Af�DAcC�Aa�TA`ffA_�PA]�AZ�yAX�AW%AVbNAU�FAT�RAT�ASVAQt�AP�`AP�/AP�AO��AN�AL�AK�#AK�7AJ  AH�AF^5AE�
AEO�AD�jAD�AA�PA@�A?��A>�!A=��A;�^A9��A89XA7A6z�A6 �A4�A3�A3�A2M�A1?}A/|�A/C�A.=qA+"�A)��A(  A&{A%S�A#�mA!|�A A�At�A�\A��A��A/A�HAv�AVA��AȴA  A`BA�A��At�A�9A^5A  A��A(�A��A�AȴA~�AffA�A��AffA��A;dA
I�A
5?A	�A	+A9XA�HA7LA�A�uAJAl�AffA Ĝ@�dZ@�j@�J@�ff@���@�S�@�~�@���@���@���@�7@��`@��m@��y@�&�@��@�Z@�Z@�ƨ@��@��@��@�hs@��/@��T@�+@�`B@� �@�@ڟ�@�n�@�v�@�^5@�{@�Q�@�@֗�@�-@���@ա�@�x�@�1@�@�V@�(�@ϝ�@�C�@�
=@���@��T@��@�Z@˥�@�S�@�K�@���@�@��@�  @�V@���@Ĵ9@�bN@� �@�1@�l�@��@�7L@��m@��@�@�x�@�7L@�j@�ƨ@�C�@�$�@��@��T@�@�-@�V@�V@���@��H@��#@�{@�5?@�ff@���@�|�@���@�v�@��7@���@�Q�@�A�@��m@�|�@�;d@���@�5?@��@���@�X@�V@���@��u@�I�@��P@�|�@�C�@��H@�5?@�@�/@���@��@���@�r�@�bN@�  @���@���@�+@���@���@�ff@�{@��@��@���@�%@���@���@���@�9X@�1@��m@��F@�|�@�S�@�K�@���@��F@��@�K�@��@���@�J@��-@��7@�x�@�`B@�&�@��/@���@�bN@��@���@�C�@�+@��@���@��\@�$�@���@�p�@�p�@�E�@�@�V@�Ĝ@���@� �@�b@�  @��@��
@���@���@��P@�dZ@�@���@��\@�{@���@�G�@�V@���@��/@�V@��@��
@���@��y@��R@���@���@��\@�@��7@�X@�7L@�V@�7L@�/@���@�Ĝ@���@�j@� �@�b@�ƨ@�dZ@�S�@�+@�"�@�;d@�+@�$�@�`B@�%@��D@�r�@�bN@�(�@��w@��P@�|�@�K�@�;d@�o@���@�M�@�n�@�v�@�^5@�@��^@���@��7@��@�hs@�/@��@��D@�(�@��F@�C�@�@�ȴ@�n�@��h@�G�@��@�;d@��@{�m@tj@l�@e��@^�@W+@O��@G��@A7L@:�H@4�j@/��@)��@$��@   @S�@��@�!@K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB49B49B49B49B49B49B5?BI�B^5Be`Bv�B�B��B�fB�B-BA�BW
B[#B]/B`BBbNBbNBffBiyB`BBYBVBM�BK�BL�BQ�BYB_;Be`Bq�Bq�By�Bu�Bk�Bt�B~�B�7B��B�-B�FB�qBȴB��B�#B�yB�fB�sB��BB	7BVBhBhBVBVBDBB��B�B�mB�)B��B��B��BƨBB�wB�3B�B��B��B�uB�DB�1B�B~�B|�B{�Bz�By�Bw�Br�Bn�BgmB_;BS�BK�B@�B49B$�BbB  B�B�HB�B��BŢB�'Bz�BffBVBC�B7LB(�B�BDB
��B
�mB
��B
�3B
��B
��B
�PB
�7B
z�B
_;B
F�B
:^B
(�B
hB
B	��B	�yB	�
B	B	�9B	��B	��B	�bB	�+B	z�B	m�B	cTB	[#B	YB	[#B	XB	W
B	S�B	N�B	L�B	K�B	I�B	D�B	>wB	9XB	8RB	7LB	7LB	0!B	'�B	%�B	$�B	#�B	!�B	�B	{B	hB	JB	B��B�B�B�B�B�B�sB�ZB�NB�;B�#B�B��B��B��B�qB�XB�9B�'B�B��B��B��B��B��B�VB�=B�DB�=B�=B�7B�+B�%B�B�B�B~�B{�Bz�By�Bw�Bu�Bt�Bs�Br�Br�Bq�Bp�Bo�Bn�Bl�Bl�BjBiyBhsBffBdZBaHB`BB_;B^5B]/B\)B[#BYBXBW
BW
BXBW
BW
BXBYBYB_;BbNBcTBffBk�Bn�By�B~�B� B�B�B�+B�VB�bB�\B�\B�DB�+B�%B�=B�PB�VB�\B�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�3B�3B�3B�9B�9B�?B�FB�XB�^B�qB�}B�}B��BŢBǮBƨBÖBĜB��B��B��B�
B�5B�fB�fB�sB�B�B�B�B�B�yB�B��B��B	  B��B��B��B��B	B	1B	\B	uB	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(�B	+B	2-B	7LB	9XB	;dB	=qB	>wB	B�B	H�B	P�B	S�B	VB	YB	[#B	]/B	`BB	aHB	cTB	cTB	e`B	gmB	iyB	k�B	o�B	p�B	u�B	v�B	w�B	x�B	y�B	x�B	z�B	z�B	z�B	{�B	{�B	}�B	�B	�1B	�7B	�1B	�+B	�%B	�+B	�+B	�+B	�+B	�1B	�7B	�JB	�PB	�bB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�3B	�3B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�jB	��B	ǮB	ƨB	ŢB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�;B	�;B	�;B	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�yB	�yB	�mB	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
%B
%B
B
+B

=B
�B
�B
#�B
+B
/B
2-B
8RB
>wB
D�B
J�B
O�B
T�B
YB
_;B
dZB
hsB
l�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B4JB4OB4OB4NB4NB4NB5RBI�B^IBeuBv�B�B�B�yB�B- BA�BWB[9B]CB`WBbdBbbBfyBi�B`VBY+BVBM�BK�BL�BRBY'B_OBesBq�Bq�By�Bu�Bk�Bt�BB�IB��B�AB�^B��B��B��B�8B�B�|B�B��B'B	KBlB}BBmBkB^B7B��B�B�B�<B�B��B��BƽB£B��B�IB�B��B��B��B�WB�DB�/BB}B|Bz�By�Bw�Br�Bn�Bg�B_RBTBK�B@�B4SB$�BwB B��B�\B�$B��BŸB�=Bz�Bf~BVBC�B7eB)B�B\B
��B
�B
��B
�LB
� B
��B
�jB
�RB
z�B
_ZB
F�B
:}B
)B
�B
>B	��B	�B	�,B	³B	�[B	��B	��B	��B	�OB	{	B	m�B	czB	[JB	Y?B	[KB	X5B	W.B	TB	OB	L�B	K�B	I�B	D�B	>�B	9�B	8yB	7tB	7sB	0JB	(B	&B	%B	$B	!�B	�B	�B	�B	tB	IB�B��B��B��B��B��B�B�B�}B�eB�PB�3B�$B�B��B��B��B�hB�UB�>B�!B�B��B��B��B��B�nB�uB�rB�nB�iB�[B�XB�LB�DB�8B-B|B{BzBxBu�Bt�Bs�Br�Br�Bq�Bp�Bo�Bn�Bl�Bl�Bj�Bi�Bh�Bf�Bd�Ba|B`wB_oB^jB]dB\]B[WBYLBXEBW=BW?BXCBW>BW?BXDBYLBYJB_pBb�Bc�Bf�Bk�Bn�BzB0B�5B�<B�LB�]B��B��B��B��B�yB�_B�XB�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�"B�!B�/B�.B�SB�[B�cB�cB�cB�jB�jB�sB�yB��B��B��B��B��B��B��B��B��B��B��B��B�B�*B�9B�eB�B�B�B�B�B��B��B�B�B��B��B�	B	 -B�B�B�
B�B	7B	`B	�B	�B	�B	�B	�B	�B	!�B	"�B	%B	'B	)"B	+.B	2ZB	7wB	9�B	;�B	=�B	>�B	B�B	H�B	QB	T#B	V0B	YBB	[PB	]ZB	`mB	asB	c~B	c|B	e�B	g�B	i�B	k�B	o�B	p�B	u�B	v�B	w�B	y B	zB	x�B	{	B	{
B	{B	|B	|B	~B	�AB	�YB	�aB	�ZB	�TB	�MB	�UB	�UB	�TB	�RB	�[B	�aB	�sB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�*B	�'B	�$B	�$B	�%B	�%B	�+B	�1B	�KB	�[B	�]B	�[B	�_B	�eB	�gB	�nB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�6B	�EB	�]B	�bB	�`B	�aB	�oB	�uB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�!B
 &B
-B
1B
8B
?B
EB
JB
KB
LB
KB
FB
QB

cB
�B
�B
#�B
+(B
/BB
2RB
8wB
>�B
D�B
J�B
PB
U"B
Y9B
_^B
d}B
h�B
l�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230509    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230509  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230509  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                