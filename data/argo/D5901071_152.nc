CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:33Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142623  20190522121827  1727_5046_152                   2C  D   APEX                            2143                            040306                          846 @��ݭ@1   @������@5�$�/��c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dz31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�33A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBXffB`ffBhffBpffBxffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C  C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fDfD�fDfD� D  D�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD� DfD�fDfD�fDfD�fDfD�fD  D�fDfD�fDfD�fDfD�fDfD�fDfD� D fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1� D2  D2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB� DCfDC�fDDfDD�fDEfDE�fDFfDF��DGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDj�Dj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo��DpfDp�fDqfDq�fDrfDr�fDsfDs�fDz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�oA��A�{A�VA�%A��A���A���A��
A���A���A�v�A�XA�XA�S�A�O�A�I�A�C�A�7LA�1'A�+A��Aǟ�A�x�A�9XA���A�dZA�E�A�ȴA���A���A¼jA��jA��A��-A���A�bNA�C�A�I�A�A�XA�$�A��A��^A�/A��yA��jA��FA��uA�&�A���A��A��+A�oA�(�A��PA���A�~�A���A��yA��jA��A�?}A��A�bNA�z�A�n�A�A�A��A�A��TA��A��+A��9A��9A�ĜA�7LA��yA�O�A��A�ffA�`BA��wA�ZA��A���A��mA�%A���A�ffA��A�-A��;A�  A��A��`A�-A��#A��A�x�A�1'A�{A�bA�ƨA��yA��9A�^5A�K�A�I�A���A~~�A~E�A~$�A}�A}7LA{�Ay�wAw��At~�Ar�jArffAq��Ao|�Al��Ak��AkAi%Ae�#Aa��A\9XAX5?ATAS�
AT�+AT9XARbNAQ\)AQ%APbNAOhsAM�;AL��ALr�AL  AK��AKC�AJ�HAJbAIVAG�AF�AD��ABM�A@ȴA?hsA>n�A=��A<�A:�`A:�A:E�A9�A8��A8r�A7ƨA7|�A7
=A6�A6=qA5�wA4�/A3t�A2r�A1�PA0v�A/�A,-A*^5A)p�A&ZA$�\A#p�A"�yA ��A%A�A��A��A�uA�
AVAl�AXA�AbNA�A�A�AoA^5A��A��A�AȴAoAAJAA��A�A=qA�PA�A  A�;A�-AdZAVA j@��@���@�{@���@��h@�?}@���@� �@��^@��
@�`B@�1@�!@��@�l�@���@�(�@�;d@�-@�O�@��m@�@�F@�w@�w@��m@㝲@�S�@◍@�p�@���@�r�@߾w@�5?@��@ݑh@��`@��@�A�@��@�O�@ݑh@ݑh@���@ۮ@���@���@�b@�dZ@��y@��@��@��@щ7@�l�@���@�+@ʧ�@ȣ�@�^5@š�@�`B@�7L@�V@ģ�@ÍP@��y@���@��`@��R@�O�@��P@�5?@�o@��^@�@��T@�p�@�/@��@��
@�dZ@�K�@�;d@��y@��\@�~�@��@��7@�7L@�j@���@�
=@�
=@���@���@�~�@�^5@�5?@���@���@��-@�@��h@�%@�b@�dZ@��H@�J@�G�@��`@��D@���@�l�@�@��R@�J@���@�I�@�S�@�S�@�+@��@���@�v�@�V@�5?@���@��T@��#@��@�$�@��@�o@�ȴ@���@�M�@�=q@�$�@�{@��@�@���@���@��7@�hs@�7L@��@���@��j@��u@���@�K�@���@��@��H@�ȴ@��!@���@��+@�V@�E�@�E�@�5?@��^@�X@���@�bN@�9X@�b@��
@��P@�33@�+@��y@���@�{@��h@��@�p�@��@���@���@��@���@�5?@�G�@�%@�%@�Ĝ@��D@�9X@��F@�\)@�C�@��R@�@��h@�O�@���@�Ĝ@��@�r�@�j@�A�@��@�;d@���@��H@�ȴ@�ff@��@��@��@��@��@���@��T@��-@���@��h@��7@��7@�p�@��@���@�r�@�r�@�r�@�j@�Q�@�9X@��@�b@���@���@��w@��@�l�@�+@�=q@��@�@�@��^@��^@��^@��h@�7L@��/@���@�1'@�ƨ@�K�@��@���@�M�@���@�`B@��@� �@�1@���@��m@�ƨ@���@�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bA�oA��A�{A�VA�%A��A���A���A��
A���A���A�v�A�XA�XA�S�A�O�A�I�A�C�A�7LA�1'A�+A��Aǟ�A�x�A�9XA���A�dZA�E�A�ȴA���A���A¼jA��jA��A��-A���A�bNA�C�A�I�A�A�XA�$�A��A��^A�/A��yA��jA��FA��uA�&�A���A��A��+A�oA�(�A��PA���A�~�A���A��yA��jA��A�?}A��A�bNA�z�A�n�A�A�A��A�A��TA��A��+A��9A��9A�ĜA�7LA��yA�O�A��A�ffA�`BA��wA�ZA��A���A��mA�%A���A�ffA��A�-A��;A�  A��A��`A�-A��#A��A�x�A�1'A�{A�bA�ƨA��yA��9A�^5A�K�A�I�A���A~~�A~E�A~$�A}�A}7LA{�Ay�wAw��At~�Ar�jArffAq��Ao|�Al��Ak��AkAi%Ae�#Aa��A\9XAX5?ATAS�
AT�+AT9XARbNAQ\)AQ%APbNAOhsAM�;AL��ALr�AL  AK��AKC�AJ�HAJbAIVAG�AF�AD��ABM�A@ȴA?hsA>n�A=��A<�A:�`A:�A:E�A9�A8��A8r�A7ƨA7|�A7
=A6�A6=qA5�wA4�/A3t�A2r�A1�PA0v�A/�A,-A*^5A)p�A&ZA$�\A#p�A"�yA ��A%A�A��A��A�uA�
AVAl�AXA�AbNA�A�A�AoA^5A��A��A�AȴAoAAJAA��A�A=qA�PA�A  A�;A�-AdZAVA j@��@���@�{@���@��h@�?}@���@� �@��^@��
@�`B@�1@�!@��@�l�@���@�(�@�;d@�-@�O�@��m@�@�F@�w@�w@��m@㝲@�S�@◍@�p�@���@�r�@߾w@�5?@��@ݑh@��`@��@�A�@��@�O�@ݑh@ݑh@���@ۮ@���@���@�b@�dZ@��y@��@��@��@щ7@�l�@���@�+@ʧ�@ȣ�@�^5@š�@�`B@�7L@�V@ģ�@ÍP@��y@���@��`@��R@�O�@��P@�5?@�o@��^@�@��T@�p�@�/@��@��
@�dZ@�K�@�;d@��y@��\@�~�@��@��7@�7L@�j@���@�
=@�
=@���@���@�~�@�^5@�5?@���@���@��-@�@��h@�%@�b@�dZ@��H@�J@�G�@��`@��D@���@�l�@�@��R@�J@���@�I�@�S�@�S�@�+@��@���@�v�@�V@�5?@���@��T@��#@��@�$�@��@�o@�ȴ@���@�M�@�=q@�$�@�{@��@�@���@���@��7@�hs@�7L@��@���@��j@��u@���@�K�@���@��@��H@�ȴ@��!@���@��+@�V@�E�@�E�@�5?@��^@�X@���@�bN@�9X@�b@��
@��P@�33@�+@��y@���@�{@��h@��@�p�@��@���@���@��@���@�5?@�G�@�%@�%@�Ĝ@��D@�9X@��F@�\)@�C�@��R@�@��h@�O�@���@�Ĝ@��@�r�@�j@�A�@��@�;d@���@��H@�ȴ@�ff@��@��@��@��@��@���@��T@��-@���@��h@��7@��7@�p�@��@���@�r�@�r�@�r�@�j@�Q�@�9X@��@�b@���@���@��w@��@�l�@�+@�=q@��@�@�@��^@��^@��^@��h@�7L@��/@���@�1'@�ƨ@�K�@��@���@�M�@���@�`B@��@� �@�1@���@��m@�ƨ@���@�X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�9B�9B�9B�9B�9B�9B�3B�3B�3B�3B�3B�3B�-B�-B�-B�-B�-B�'B�'B�!B�B�B�B�B�B�B�B�B�wB�BN�B.B��BBN�B^5BbNBq�B��B��B�XB�jB�qBȴB��B��B��B��B��BɺB��BǮBŢBɺB�B�TB�sB�B�B�B�B�B�B�B�ZB�/B�B�/B�HB��B�'B�\Bt�BZBI�BM�BJ�B;dB,B%�B+B&�B$�B�B�B1B�B�TBȴB��B�^B��B�BXB%�B�BPBBBB
��B
��B
��B
�mB
�B
ŢB
�B
�=B
gmB
O�B
>wB
=qB
;dB
9XB
33B
(�B
�B
PB
  B	��B	��B	�B	�mB	�#B	�B	��B	�}B	��B	��B	s�B	XB	D�B	I�B	e`B	q�B	jB	gmB	jB	hsB	gmB	aHB	`BB	^5B	]/B	[#B	ZB	YB	W
B	Q�B	L�B	F�B	<jB	)�B	!�B	�B	�B	�B	uB	oB	uB	hB	VB	PB	VB	JB		7B	%B	B	B	+B	%B��B��B�B�B�HB��BB�jB�B��B��B��B��B�JB�DB�B}�Bw�Bt�Bt�Bw�B�B�B�B�B}�By�Bv�Bu�Br�Bp�Bm�BjBdZB_;B_;B^5B]/B\)B[#BYBYBZBYBYBW
BT�BO�BN�BO�BP�BP�BP�BP�BP�BP�BR�BR�BS�BS�BS�BR�BS�B[#B\)B\)B^5B^5B`BBaHBaHB`BB`BB`BB`BB_;B_;B^5B^5B]/B]/B`BB_;B`BBaHBe`Br�Bz�B{�B~�B~�B}�B~�Bz�Bv�Bw�Bv�Bv�Bv�B}�B�+B�+B�JB�PB�PB�\B�{B�hB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�9B�LB�RB�RB�XB�XB�jB��BBÖBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�#B�5B�sB��B	B	
=B	PB	\B	{B	�B	�B	�B	{B	hB	bB	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	'�B	+B	-B	.B	1'B	2-B	33B	33B	6FB	8RB	9XB	:^B	;dB	=qB	@�B	A�B	B�B	E�B	G�B	O�B	ZB	^5B	_;B	`BB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	ffB	jB	l�B	q�B	r�B	s�B	s�B	t�B	u�B	w�B	w�B	x�B	y�B	y�B	z�B	y�B	y�B	y�B	x�B	y�B	{�B	|�B	}�B	�B	�B	�B	�%B	�+B	�7B	�=B	�7B	�7B	�1B	�DB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�9B	�9B	�?B	�FB	�RB	�dB	�jB	�qB	�wB	�}B	��B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�)B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�9B�9B�9B�9B�9B�9B�?B�3B�3B�3B�3B�?B�9B�-B�-B�-B�-B�-B�'B�'B�!B�!B�B�B�B�B�B�B�-B�qB�BW
B6FB��BBO�B_;BbNBq�B��B�B�^B�qB�wB��B��B��B��B��B��B��B��BȴBǮB��B�)B�`B�B�B��B�B�B�B�B��B�yB�NB�B�;B�sB�5B��B�{Bw�B^5BM�BO�BO�B>wB.B'�B/B)�B&�B �B!�BbB��B�B��BÖB�}B�3B�uBgmB-B�B{BBBBBB
��B
�B
�5B
��B
�RB
�uB
n�B
W
B
?}B
=qB
<jB
;dB
6FB
/B
 �B
�B
B	��B	��B	��B	�B	�5B	�#B	�B	ǮB	�!B	��B	~�B	dZB	D�B	G�B	ffB	w�B	m�B	hsB	l�B	jB	jB	cTB	aHB	_;B	^5B	\)B	[#B	[#B	ZB	T�B	O�B	L�B	B�B	.B	%�B	"�B	�B	�B	�B	uB	{B	oB	hB	VB	bB	PB	
=B	+B	B	%B	
=B	
=B	B��B��B�B�B��BŢBŢB�'B��B��B��B��B�oB�\B�=B�By�Bw�By�Bw�B�B�B�B�B�B{�Bx�Bw�Bt�Bs�Bo�Bn�Bl�BdZB`BB_;B_;B^5B]/B\)B[#B[#BZBZBXBW
BW
BP�BP�BQ�BQ�BQ�BQ�BQ�BT�BVBW
BVBVBW
BVBXB\)B^5B^5B_;B`BB`BBaHBaHB`BB`BBaHBaHBaHBaHB_;B_;B_;B`BBaHB`BBbNBdZBcTBq�Bz�B{�B~�B� B� B� B� Bz�Bx�Bw�Bw�Bv�B|�B�7B�=B�bB�\B�VB�oB��B�oB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�?B�RB�XB�RB�XB�^B�qB��BBĜBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�)B�;B�yB��B	B	DB	VB	bB	�B	�B	�B	�B	�B	oB	bB	oB	{B	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	'�B	+B	-B	/B	1'B	2-B	33B	33B	6FB	8RB	9XB	:^B	;dB	=qB	@�B	A�B	C�B	E�B	H�B	P�B	[#B	^5B	_;B	`BB	bNB	cTB	dZB	e`B	ffB	gmB	gmB	gmB	k�B	m�B	r�B	r�B	s�B	s�B	u�B	v�B	w�B	x�B	y�B	y�B	z�B	z�B	y�B	y�B	y�B	y�B	z�B	{�B	}�B	~�B	�B	�B	�B	�%B	�+B	�=B	�DB	�7B	�=B	�7B	�JB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�9B	�9B	�FB	�LB	�RB	�dB	�jB	�qB	�wB	�}B	��B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<D��<u<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.1 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447272012010314472720120103144727  AO  ARGQ                                                                        20111130142623  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142623  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144727  IP                  G�O�G�O�G�O�                