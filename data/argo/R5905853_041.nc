CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:29:59Z creation;2022-06-04T17:29:59Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172959  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�P/�b�1   @�P�i�@0$�/��b�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  @���A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  BǙ�B�33B�  B�33B�33B�  B�  B�33B���B�  B�  B�  B�  B�  C   C  C  C  CL�C	�fC��C  C  C  C�fC  C  C  C�C  C   C"  C$  C&  C(L�C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@n{@�
=@��
A�A;�A[�A{�A�A��\A�A�A�A�A�A�B�HB�HBG�B�HB&�HB.�HB6�HB>�HBF�HBN�HBV�HB^�HBf�HBn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�
>Bʣ�B�p�Bӣ�Bף�B�p�B�p�B��B�=qB�p�B�p�B�p�B�p�B�p�B�p�C�RC�RC�RCC	��C�C�RC�RC�RC��C�RC�RC�RC��C�RC�RC!�RC#�RC%�RC(C)��C+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS��CU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
nD
�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D nD �D!nD!�D"nD"�D#nD#�D$nD$�D%nD%�D&nD&�D'nD'�D(nD(�D)nD)�D*nD*�D+nD+�D,nD,�D-nD-�D.nD.�D/nD/�D0nD0�D1nD1�D2nD2�D3nD3�D4nD4�D5nD5�D6nD6�D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�D=nD=�D>nD>�D?nD?�D@nD@�DAnDA�DBnDB�DCnDC�DDnDD�DEnDE�DFnDF�DGnDG�DHnDH�DInDI�DJnDJ�DKnDK�DLnDL�DMnDM�DNnDN�DOnDO�DPnDP�DQnDQ�DRnDR�DSnDS�DTnDT�DUnDU�DVnDV�DWnDW�DXnDX�DYnDY�DZnDZ�D[nD[�D\nD\�D]nD]�D^nD^�D_nD_�D`nD`�DanDa�DbnDb�DcnDc�DdnDd�DenDe�DfnDf�DgnDg�DhnDh�DinDi�DjnDj�DknDk�DlnDl�DmnDm�DnnDn�DonDo�DpnDp�DqnDq�DrnDr�DsnDs�DtnDt�DunDu�DvnDv�DwnDw�DxnDx�DynDy�DznDz�D{nD{�D|nD|�D}nD}�D~nD~�DnD�D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�z=D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D·
D��
D�7
D�w
D÷
D��
D�7
D�w
Dķ
D��
D�7
D�w
Dŷ
D��
D�7
D�w
DƷ
D��
D�7
D�w
DǷ
D��
D�7
D�w
Dȷ
D��
D�7
D�w
Dɷ
D��
D�7
D�w
Dʷ
D��
D�7
D�w
D˷
D��
D�7
D�w
D̷
D��
D�7
D�w
Dͷ
D��
D�7
D�w
Dη
D��
D�7
D�w
DϷ
D��
D�7
D�w
Dз
D��
D�7
D�w
Dѷ
D��
D�7
D�w
Dҷ
D��
D�7
D�w
Dӷ
D��
D�7
D�w
DԷ
D��
D�7
D�w
Dշ
D��
D�7
D�w
Dַ
D��
D�7
D�w
D׷
D��
D�7
D�w
Dط
D��
D�7
D�w
Dٷ
D��
D�7
D�w
Dڷ
D��
D�7
D�w
D۷
D��
D�7
D�w
Dܷ
D��
D�7
D�w
Dݷ
D��
D�7
D�w
D޷
D��
D�7
D�w
D߷
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D��
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D�
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�3�D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
D��
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=<A�E9A�FA�E�A�EA�C�A�IA�J�A�LdA�HA�=<A�?�A�F?A�K�A�EA�>wA�@OA�>�A�:*A�?HA�:�A�,A�*0A�'�A�CA��A�
�A��A��A� iA���A���A��NAź�AœA�i�A�R�A�=�A�-�A�IA�
	A���A���A��ZA���A��[A���AļjA�
�A�hA�aHA��A���A�l�A���A���A�xA�<6A�G�A��1A��5A���A�"A�~�A�^A�~�A�'RA�TaA�y�A�k�A��!A��5A�Z�A�poA��A���A�P}A��A�<6A�[�A�A|�Aw�fAtk�Ams�Aj�
Ai��Af��Ae �Ac��A`�A^"hAZv`AX'�AV�AU�ATS�AQO�AOVAK�ZAJ�AH��AH/�AH�AB{A>ϫA>��A>�YA="hA;1'A;�A9�=A3��A0��A1y�A3	A2xlA0�"A-�"A)�4A'A&W�A&4A%�sA&	lA&zxA&ƨA&�AA(�hA)�_A)��A)m]A(��A'��A'��A'��A&�kA%u�A%6�A$�'A$s�A#�A#�A#�A#��A#��A#U�A"A!+A E9A��A)�A�zAqvAZ�A !-A �A�A�A��Al�Ap;A'�A:*A$�A��A:�A��A{�A�)AC-AYA�&A��AjA($A�aAQA_A�A�0A�A��A�At�A1�A��A�+AXyA�#AFA��A�CAm�AGEA�A�6AkQA�A�vAl"A�KA�A8�A��A��AXyAA A	lA��A=�A�	A��A4�A
�	A
tTA	�mA	^5A��AJ#A��A��Au%A.�A�AI�A��A��AFtAOA��Ac�A;dA�A�A�jA��A��A~(A�A��A�XA��A`�A@�A+A�A�HA��Ah
A7�A�A �)A ��A N�A 	@��w@�,�@��@���@�Ĝ@� �@�F�@��@��+@��K@��V@�e�@�.I@��@��.@��@��5@�l"@���@�y�@��@��s@�_@��>@�j�@��@���@�@�r@��@��@�4@���@�4n@���@�K�@�w�@���@��2@�c�@�7@���@�Y�@��y@�h�@�f�@藍@�w�@�	�@��@�H�@��@�/�@�H@�8@�y>@��@᫟@�+@��c@�Ov@ߊ	@���@���@�hs@��@�K^@ۼ@�9�@ڦL@�bN@�!@���@�2a@�W�@א�@ׄM@��@֚@��@�o @ԯO@��o@ӍP@Ҵ9@�m�@�Q@�4@�_p@��@��@��@ΆY@�	�@́�@�$t@��|@̬@�}V@� �@��,@���@ʿ�@��@��T@Ƀ{@�_p@��@Ȱ!@�PH@��A@Ǖ�@���@���@�t�@��@�ff@�>B@�x@��@�~�@��y@�M�@��)@��h@�@@���@�`�@���@�8�@���@��@���@��{@�y�@�dZ@��@��@�ff@�(�@�ϫ@��S@�e,@�8@���@�w�@��@���@���@�iD@�=�@��@���@�1'@��@��@��@�ƨ@�j@��@��P@�u�@��Z@�ϫ@��^@���@�.I@���@��_@�N�@��'@��H@��1@�!�@�}�@�@��@�z@�2�@��@�@��@��@���@�rG@�Vm@��	@�C�@���@���@�P�@�4@��@���@�5?@��X@�s@�F@���@���@�]d@��@���@���@�\�@�o@��	@���@��@�<�@���@��f@�f�@��`@��@�z�@�-�@��.@��;@���@�]�@�@@��+@�w�@�1�@�p�@���@�p;@���@���@�S&@�(@��@���@�h�@�*�@���@�a�@���@�B[@�7@��)@��3@���@�g�@���@���@�L0@���@�l�@�k�@��=@��@�_p@�8@��@���@��p@��6@�{�@�e�@�:*@��@��H@��{@�X�@�(@���@�e�@�D�@�  @���@���@�&@��!@�z�@�A�@��@�l�@��f@��,@��@�5?@��H@�zx@�J#@�:�@��$@�oi@�'R@��M@��@���@���@�y>@�e�@�Ta@�)�@��#@���@���@��@�e,@�7L@�	l@��m@�bN@�K^@�4@���@��7@�a@�@@�u�@�,=@���@���@�1�@���@���@���@��\@�l�@�"h@��@��@�N<@�;@���@�w�@�:�@��@��@���@�J#@��M@��j@��.@�[�@�($@��m@���@���@�y�@�5�@�;@��@�ی@���@�Q@�'R@��@��@��m@��H@��f@�,�@�S@���@��H@��X@��h@���@�ff@�!@��3@���@�c@�x@�rG@�5�@�ی@�|�@�a|@�5?@�x@�+@��@~�,@~�!@~L0@}�@}�h@}#�@|��@|�@|�@|,=@{��@{l�@z��@zYK@z#:@y}�@yQ�@y0�@yq@x�5@x��@x1'@w��@w�@v	@u��@u��@uX@u�@t�f@t��@t��@tH@s��@sE9@r��@rl�@r	@q��@p��@p��@p��@p"h@o��@o4�@o$t@n�y@n?@m��@m2a@m�@l�?@l"h@k��@ko�@j�y@j�@ju%@j.�@i�@i��@i&�@h��@h/�@g��@g��@ga@f�s@fC�@f{@e��@ec�@e:�@dѷ@d�@c@b��@b)�@b	@a�9@a��@aF@a�@a�@`��@`Ĝ@`��@`>B@_�;@_e�@_A�@^�}@^Q@^.�@^ �@]��@]�^@]:�@\�4@\S�@\Z@[�P@[o@[�@[�@Z~�@ZB[@Z.�@Y��@X��@X�O@X�j@X�I@Xz�@X�@W�0@W@V��@VR�@U�)@U�@Us�@UN<@T֡@TQ�@T�@S�;@S��@SRT@S�@R�'@R~�@RkQ@R=q@R�@Q@Q^�@Q%F@PZ@P1@OK�@N�X@Nl�@M��@M}�@M�@L��@K��@KH�@K
=@J��@JM�@J_@I��@I��@I}�@I�@H�z@Hl"@H�@G;d@Fxl@F�@E��@E��@EIR@D�[@D��@D�@Dq@D�@C_p@C+@B��@B�R@B�@A[W@@�@?� @?�@>L0@=��@=J�@=+�@=@<�@<�?@<�@<�@;��@;�@:�y@:��@:!�@9�-@9rG@9�@8m�@7خ@7�@7x@7�@6�R@6{�@64@6 �@5�)@5��@5�D@6�@6	@6\�@6��@6�R@6�@6��@6��@6�x@6xl@6kQ@6?@6 �@5��@5�@5�"@5�@4�U@4��@4oi@44n@3�+@3�P@3 i@2��@2:*@2?@1�@18�@0��@0�)@0>B@0x@/�@/b�@.��@.R�@-��@-�-@-|@-(�@,��@,z�@+�@+��@+�w@+��@+dZ@+C@*�,@*q�@)�.@)��@)[W@)?}@)+�@(�|@(�$@(:�@'�K@'�F@'��@'W?@'H�@'H�@'�@&��@&O@%�z@%�C@%�=@%x�@% \@$�e@$��@$~(@$K^@$�@$1@#�&@#��@#�P@#{J@#S�@#�@"��@"��@"@�@!��@!�@!�@!��@!u�@!X@!8�@! \@ �[@ �@ $@�q@�P@~�@U�@S@�H@�'@��@h
@@�@��@S&@#�@�P@ی@Ɇ@�4@Q�@��@��@E9@��@�s@u%@($@ �@�N@�n@��@c@:�@�[@��@��@c�@:�@~@�@�*@o�@U�@6z@�@�@�]@�L@�+@n�@V@J@�@��@zx@F@*0@@@��@��@|�@h�@"h@�@��@�P@Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=<A�E9A�FA�E�A�EA�C�A�IA�J�A�LdA�HA�=<A�?�A�F?A�K�A�EA�>wA�@OA�>�A�:*A�?HA�:�A�,A�*0A�'�A�CA��A�
�A��A��A� iA���A���A��NAź�AœA�i�A�R�A�=�A�-�A�IA�
	A���A���A��ZA���A��[A���AļjA�
�A�hA�aHA��A���A�l�A���A���A�xA�<6A�G�A��1A��5A���A�"A�~�A�^A�~�A�'RA�TaA�y�A�k�A��!A��5A�Z�A�poA��A���A�P}A��A�<6A�[�A�A|�Aw�fAtk�Ams�Aj�
Ai��Af��Ae �Ac��A`�A^"hAZv`AX'�AV�AU�ATS�AQO�AOVAK�ZAJ�AH��AH/�AH�AB{A>ϫA>��A>�YA="hA;1'A;�A9�=A3��A0��A1y�A3	A2xlA0�"A-�"A)�4A'A&W�A&4A%�sA&	lA&zxA&ƨA&�AA(�hA)�_A)��A)m]A(��A'��A'��A'��A&�kA%u�A%6�A$�'A$s�A#�A#�A#�A#��A#��A#U�A"A!+A E9A��A)�A�zAqvAZ�A !-A �A�A�A��Al�Ap;A'�A:*A$�A��A:�A��A{�A�)AC-AYA�&A��AjA($A�aAQA_A�A�0A�A��A�At�A1�A��A�+AXyA�#AFA��A�CAm�AGEA�A�6AkQA�A�vAl"A�KA�A8�A��A��AXyAA A	lA��A=�A�	A��A4�A
�	A
tTA	�mA	^5A��AJ#A��A��Au%A.�A�AI�A��A��AFtAOA��Ac�A;dA�A�A�jA��A��A~(A�A��A�XA��A`�A@�A+A�A�HA��Ah
A7�A�A �)A ��A N�A 	@��w@�,�@��@���@�Ĝ@� �@�F�@��@��+@��K@��V@�e�@�.I@��@��.@��@��5@�l"@���@�y�@��@��s@�_@��>@�j�@��@���@�@�r@��@��@�4@���@�4n@���@�K�@�w�@���@��2@�c�@�7@���@�Y�@��y@�h�@�f�@藍@�w�@�	�@��@�H�@��@�/�@�H@�8@�y>@��@᫟@�+@��c@�Ov@ߊ	@���@���@�hs@��@�K^@ۼ@�9�@ڦL@�bN@�!@���@�2a@�W�@א�@ׄM@��@֚@��@�o @ԯO@��o@ӍP@Ҵ9@�m�@�Q@�4@�_p@��@��@��@ΆY@�	�@́�@�$t@��|@̬@�}V@� �@��,@���@ʿ�@��@��T@Ƀ{@�_p@��@Ȱ!@�PH@��A@Ǖ�@���@���@�t�@��@�ff@�>B@�x@��@�~�@��y@�M�@��)@��h@�@@���@�`�@���@�8�@���@��@���@��{@�y�@�dZ@��@��@�ff@�(�@�ϫ@��S@�e,@�8@���@�w�@��@���@���@�iD@�=�@��@���@�1'@��@��@��@�ƨ@�j@��@��P@�u�@��Z@�ϫ@��^@���@�.I@���@��_@�N�@��'@��H@��1@�!�@�}�@�@��@�z@�2�@��@�@��@��@���@�rG@�Vm@��	@�C�@���@���@�P�@�4@��@���@�5?@��X@�s@�F@���@���@�]d@��@���@���@�\�@�o@��	@���@��@�<�@���@��f@�f�@��`@��@�z�@�-�@��.@��;@���@�]�@�@@��+@�w�@�1�@�p�@���@�p;@���@���@�S&@�(@��@���@�h�@�*�@���@�a�@���@�B[@�7@��)@��3@���@�g�@���@���@�L0@���@�l�@�k�@��=@��@�_p@�8@��@���@��p@��6@�{�@�e�@�:*@��@��H@��{@�X�@�(@���@�e�@�D�@�  @���@���@�&@��!@�z�@�A�@��@�l�@��f@��,@��@�5?@��H@�zx@�J#@�:�@��$@�oi@�'R@��M@��@���@���@�y>@�e�@�Ta@�)�@��#@���@���@��@�e,@�7L@�	l@��m@�bN@�K^@�4@���@��7@�a@�@@�u�@�,=@���@���@�1�@���@���@���@��\@�l�@�"h@��@��@�N<@�;@���@�w�@�:�@��@��@���@�J#@��M@��j@��.@�[�@�($@��m@���@���@�y�@�5�@�;@��@�ی@���@�Q@�'R@��@��@��m@��H@��f@�,�@�S@���@��H@��X@��h@���@�ff@�!@��3@���@�c@�x@�rG@�5�@�ی@�|�@�a|@�5?@�x@�+@��@~�,@~�!@~L0@}�@}�h@}#�@|��@|�@|�@|,=@{��@{l�@z��@zYK@z#:@y}�@yQ�@y0�@yq@x�5@x��@x1'@w��@w�@v	@u��@u��@uX@u�@t�f@t��@t��@tH@s��@sE9@r��@rl�@r	@q��@p��@p��@p��@p"h@o��@o4�@o$t@n�y@n?@m��@m2a@m�@l�?@l"h@k��@ko�@j�y@j�@ju%@j.�@i�@i��@i&�@h��@h/�@g��@g��@ga@f�s@fC�@f{@e��@ec�@e:�@dѷ@d�@c@b��@b)�@b	@a�9@a��@aF@a�@a�@`��@`Ĝ@`��@`>B@_�;@_e�@_A�@^�}@^Q@^.�@^ �@]��@]�^@]:�@\�4@\S�@\Z@[�P@[o@[�@[�@Z~�@ZB[@Z.�@Y��@X��@X�O@X�j@X�I@Xz�@X�@W�0@W@V��@VR�@U�)@U�@Us�@UN<@T֡@TQ�@T�@S�;@S��@SRT@S�@R�'@R~�@RkQ@R=q@R�@Q@Q^�@Q%F@PZ@P1@OK�@N�X@Nl�@M��@M}�@M�@L��@K��@KH�@K
=@J��@JM�@J_@I��@I��@I}�@I�@H�z@Hl"@H�@G;d@Fxl@F�@E��@E��@EIR@D�[@D��@D�@Dq@D�@C_p@C+@B��@B�R@B�@A[W@@�@?� @?�@>L0@=��@=J�@=+�@=@<�@<�?@<�@<�@;��@;�@:�y@:��@:!�@9�-@9rG@9�@8m�@7خ@7�@7x@7�@6�R@6{�@64@6 �@5�)@5��@5�D@6�@6	@6\�@6��@6�R@6�@6��@6��@6�x@6xl@6kQ@6?@6 �@5��@5�@5�"@5�@4�U@4��@4oi@44n@3�+@3�P@3 i@2��@2:*@2?@1�@18�@0��@0�)@0>B@0x@/�@/b�@.��@.R�@-��@-�-@-|@-(�@,��@,z�@+�@+��@+�w@+��@+dZ@+C@*�,@*q�@)�.@)��@)[W@)?}@)+�@(�|@(�$@(:�@'�K@'�F@'��@'W?@'H�@'H�@'�@&��@&O@%�z@%�C@%�=@%x�@% \@$�e@$��@$~(@$K^@$�@$1@#�&@#��@#�P@#{J@#S�@#�@"��@"��@"@�@!��@!�@!�@!��@!u�@!X@!8�@! \@ �[@ �@ $@�q@�P@~�@U�@S@�H@�'@��@h
@@�@��@S&@#�@�P@ی@Ɇ@�4@Q�@��@��@E9@��@�s@u%@($@ �@�N@�n@��@c@:�@�[@��@��@c�@:�@~@�@�*@o�@U�@6z@�@�@�]@�L@�+@n�@V@J@�@��@zx@F@*0@@@��@��@|�@h�@"h@�@��@�P@Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BՁB�gB�gBՁB՛BՁB�gB�gB�gB՛B՛B՛BյBՁBյBՁB՛B՛B՛BյBյB��B��B�B�B�B�B�SBևB��B�YB��B�yBؓB�B��BڠB�=B�=B�	B��B��BؓB�1B�QB�qB�)B��B��B�YB
T{B
��B
��B
�oB
�cB
��B
�kB
ߊB
�B
�ZB
�=B
�sB
�B
�lB
B
�B
�ZB
�B
��B
��B
t�B
f�B
c B
R�B	�B	ΥB	�tB	�nB	�KB	��B	|B	oB	Z�B	DB	)B	;B	 �B	�B	B	)�B	%�B	 \B	�B	;B��B	B	�B	�B��B��B�DB�BB	�B	5�B	/�B	*�B	88B	=�B	IB	G_B	YeB	TB	1�B	0�B	b4B	�aB	��B	�B	v+B	Z�B	LJB	MPB	M�B	N<B	\�B	r|B	��B	��B	οB	��B
�B
FB
2B
B
�B
+�B
-�B
-wB
-�B
/OB
0UB
1[B
7�B
<PB
?�B
B�B
D�B
@�B
8�B
2aB
/�B
+�B
)�B
7B
BuB
S&B
R�B
K�B
9�B
7�B
9�B
=�B
C�B
JXB
M�B
L�B
MB
L�B
OBB
T�B
T�B
S�B
T,B
TFB
UB
T�B
TFB
W
B
T�B
X�B
X_B
WYB
X�B
Y�B
X+B
YKB
XEB
ZB
[�B
\�B
[�B
\CB
\CB
[=B
\xB
]IB
]�B
[�B
[�B
[	B
ZB
XyB
W�B
XB
WsB
V�B
VB
U�B
U�B
UgB
T�B
S�B
R�B
RoB
R�B
Q�B
PB
NVB
L�B
J�B
I�B
H�B
G�B
F�B
E�B
D�B
C�B
B�B
A�B
A;B
@�B
?�B
?B
>�B
>BB
=�B
=�B
="B
=B
<jB
;�B
;�B
;JB
:�B
:xB
:�B
:�B
;B
:�B
:B
9>B
8�B
7�B
7B
6`B
5?B
4�B
4�B
4B
3�B
3�B
3MB
2-B
1�B
1�B
1B
0�B
0�B
0�B
0;B
/�B
/5B
.B
-]B
,B
+B
+B
*�B
)�B
)�B
)�B
)_B
)DB
)DB
)B
(�B
)_B
(�B
(>B
'�B
'B
&B
$ZB
#�B
"B
!�B
!-B
 �B
�B
!B
B
�B
�B
�B
 �B
�B
�B
�B
�B
qB
�B

B
�B
SB
B
�B
�B
�B
�B
&B
B
�B
�B
oB
�B
�B
�B
�B
�B
BB
BB
<B
�B
�B
�B
B
jB
�B
�B
VB
pB
�B
�B
�B
PB
B
�B
DB

	B
	�B
	�B
	�B
B
�B
6B
0B
�B
	�B

XB
)B

�B
DB
^B
B

�B

=B

	B

=B
	lB
	B
	lB
	RB
	�B
	RB
	B
�B
�B
	lB
	�B
	7B
	�B
	�B
	7B
	7B
	�B
	�B

=B

rB

XB

XB

XB

=B

�B

�B

rB

�B
)B
B
^B
xB
�B
�B
jB
6B
PB
PB
�B
�B
"B
VB
VB
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
B
TB
�B
�B
 B
�B
 B
 B
 B
�B
oB
oB
oB
TB
TB
TB
B
TB
:B
oB
B
[B
�B
�B
�B
aB
�B
�B
�B
B
@B
�B
 B
�B
�B
�B
@B
�B
�B
aB
�B
B
SB
mB

B

B
YB
YB
$B
$B
?B

B
�B
�B
�B
B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
TB
:B
�B
�B
�B
}B
.B
�B
\B
BB
�B
B
�B
BB
HB
oB
�B
+B
_B
KB
KB
eB
�B
B
7B
kB
�B
�B
=B
#B
WB
�B
#B
	B
�B
qB
	B
�B
�B
eB
�B
�B
�B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
 �B
!-B
!|B
!�B
!�B
!�B
!�B
!�B
"hB
"hB
"�B
"�B
"�B
#B
#TB
#�B
$B
$@B
$@B
$ZB
$�B
$�B
%FB
&B
%�B
&fB
&�B
&�B
'mB
'�B
'�B
(>B
(XB
(�B
(�B
)B
)�B
*KB
+B
+�B
,�B
,�B
-)B
-�B
.IB
.}B
/ B
/OB
/�B
/iB
/�B
0;B
0;B
0�B
1vB
1�B
1�B
1�B
1�B
2|B
2|B
2|B
2aB
2�B
2�B
2�B
3MB
3MB
3MB
3MB
3hB
3hB
3MB
3MB
3B
3hB
3�B
3�B
3hB
3MB
3hB
3�B
4B
4B
4nB
4�B
4�B
5B
5ZB
5?B
5�B
5�B
5�B
5�B
5�B
6+B
6B
6FB
6zB
6�B
7B
72B
7LB
7�B
7�B
7�B
7�B
7�B
8B
88B
8B
9	B
9�B
9�B
9�B
:B
:^B
:DB
:^B
:DB
:�B
;JB
;0B
;B
;�B
;�B
<B
=B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?}B
?�B
?�B
?�B
?�B
@ B
@ B
@B
@B
@OB
@iB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
AB
@�B
A;B
A�B
AoB
A�B
B'B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
D3B
DMB
D�B
E9B
E�B
F�B
G�B
HB
G�B
G+B
F�B
G�B
F�B
F�B
F�B
F�B
GB
G�B
HfB
H�B
H�B
HfB
J#B
J�B
KB
K^B
KxB
J�B
J�B
J�B
K�B
L0B
LdB
L�B
M6B
MPB
MPB
M�B
N"B
NpB
NpB
N�B
N�B
N�B
OB
O(B
OBB
OvB
OvB
PB
P.B
P�B
P�B
QB
Q�B
Q�B
RB
RB
R�B
R�B
R:B
R B
R B
R:B
R:B
RTB
R�B
SB
S&B
SB
S[B
S�B
S�B
S�B
TFB
U2B
UMB
U�B
U�B
U�B
VB
VB
VSB
V�B
VmB
V�B
V9B
U�B
UB
T�B
UMB
T�B
T{B
TFB
S�B
T,B
TB
TB
T,B
T�B
T�B
T�B
U�B
U�B
U�B
V9B
VB
VSB
V�B
WsB
W�B
XB
XyB
X�B
YB
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
[WB
\�B
^B
^�B
_�B
_�B
_pB
`�B
`�B
aHB
a�B
a�B
a�B
a�B
a�B
bNB
b�B
bhB
bNB
b�B
bhB
bhB
b4B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
bhB
bNB
bhB
bNB
bhB
b�B
cB
cTB
c:B
c�B
c�B
dB
d�B
d�B
ezB
e`B
ezB
e�B
e�B
ffB
ffB
f�B
f�B
f�B
gB
g8B
g�B
g�B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
iyB
i�B
j0B
j0B
jKB
jeB
j�B
k6B
k6B
k6B
k�B
k�B
k�B
k�B
l=B
l"B
l=B
l=B
l�B
l�B
l�B
m)B
m]B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
nB
nIB
n�B
o B
oB
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
qB
p�B
qAB
qAB
qAB
q�B
rB
r-B
raB
r�B
r�B
sB
shB
shB
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
uB
u%B
u%B
u�B
u�B
v+B
vFB
vFB
vFB
vzB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xlB
xRB
x�B
x�B
y$B
y$B
yrB
y�B
y�B
z*B
zDB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BՁB�gB�gBՁB՛BՁB�gB�gB�gB՛B՛B՛BյBՁBյBՁB՛B՛B՛BյBյB��B��B�B�B�B�B�SBևB��B�YB��B�yBؓB�B��BڠB�=B�=B�	B��B��BؓB�1B�QB�qB�)B��B��B�YB
T{B
��B
��B
�oB
�cB
��B
�kB
ߊB
�B
�ZB
�=B
�sB
�B
�lB
B
�B
�ZB
�B
��B
��B
t�B
f�B
c B
R�B	�B	ΥB	�tB	�nB	�KB	��B	|B	oB	Z�B	DB	)B	;B	 �B	�B	B	)�B	%�B	 \B	�B	;B��B	B	�B	�B��B��B�DB�BB	�B	5�B	/�B	*�B	88B	=�B	IB	G_B	YeB	TB	1�B	0�B	b4B	�aB	��B	�B	v+B	Z�B	LJB	MPB	M�B	N<B	\�B	r|B	��B	��B	οB	��B
�B
FB
2B
B
�B
+�B
-�B
-wB
-�B
/OB
0UB
1[B
7�B
<PB
?�B
B�B
D�B
@�B
8�B
2aB
/�B
+�B
)�B
7B
BuB
S&B
R�B
K�B
9�B
7�B
9�B
=�B
C�B
JXB
M�B
L�B
MB
L�B
OBB
T�B
T�B
S�B
T,B
TFB
UB
T�B
TFB
W
B
T�B
X�B
X_B
WYB
X�B
Y�B
X+B
YKB
XEB
ZB
[�B
\�B
[�B
\CB
\CB
[=B
\xB
]IB
]�B
[�B
[�B
[	B
ZB
XyB
W�B
XB
WsB
V�B
VB
U�B
U�B
UgB
T�B
S�B
R�B
RoB
R�B
Q�B
PB
NVB
L�B
J�B
I�B
H�B
G�B
F�B
E�B
D�B
C�B
B�B
A�B
A;B
@�B
?�B
?B
>�B
>BB
=�B
=�B
="B
=B
<jB
;�B
;�B
;JB
:�B
:xB
:�B
:�B
;B
:�B
:B
9>B
8�B
7�B
7B
6`B
5?B
4�B
4�B
4B
3�B
3�B
3MB
2-B
1�B
1�B
1B
0�B
0�B
0�B
0;B
/�B
/5B
.B
-]B
,B
+B
+B
*�B
)�B
)�B
)�B
)_B
)DB
)DB
)B
(�B
)_B
(�B
(>B
'�B
'B
&B
$ZB
#�B
"B
!�B
!-B
 �B
�B
!B
B
�B
�B
�B
 �B
�B
�B
�B
�B
qB
�B

B
�B
SB
B
�B
�B
�B
�B
&B
B
�B
�B
oB
�B
�B
�B
�B
�B
BB
BB
<B
�B
�B
�B
B
jB
�B
�B
VB
pB
�B
�B
�B
PB
B
�B
DB

	B
	�B
	�B
	�B
B
�B
6B
0B
�B
	�B

XB
)B

�B
DB
^B
B

�B

=B

	B

=B
	lB
	B
	lB
	RB
	�B
	RB
	B
�B
�B
	lB
	�B
	7B
	�B
	�B
	7B
	7B
	�B
	�B

=B

rB

XB

XB

XB

=B

�B

�B

rB

�B
)B
B
^B
xB
�B
�B
jB
6B
PB
PB
�B
�B
"B
VB
VB
�B
�B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
�B
�B
B
TB
�B
�B
 B
�B
 B
 B
 B
�B
oB
oB
oB
TB
TB
TB
B
TB
:B
oB
B
[B
�B
�B
�B
aB
�B
�B
�B
B
@B
�B
 B
�B
�B
�B
@B
�B
�B
aB
�B
B
SB
mB

B

B
YB
YB
$B
$B
?B

B
�B
�B
�B
B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
TB
:B
�B
�B
�B
}B
.B
�B
\B
BB
�B
B
�B
BB
HB
oB
�B
+B
_B
KB
KB
eB
�B
B
7B
kB
�B
�B
=B
#B
WB
�B
#B
	B
�B
qB
	B
�B
�B
eB
�B
�B
�B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
 �B
!-B
!|B
!�B
!�B
!�B
!�B
!�B
"hB
"hB
"�B
"�B
"�B
#B
#TB
#�B
$B
$@B
$@B
$ZB
$�B
$�B
%FB
&B
%�B
&fB
&�B
&�B
'mB
'�B
'�B
(>B
(XB
(�B
(�B
)B
)�B
*KB
+B
+�B
,�B
,�B
-)B
-�B
.IB
.}B
/ B
/OB
/�B
/iB
/�B
0;B
0;B
0�B
1vB
1�B
1�B
1�B
1�B
2|B
2|B
2|B
2aB
2�B
2�B
2�B
3MB
3MB
3MB
3MB
3hB
3hB
3MB
3MB
3B
3hB
3�B
3�B
3hB
3MB
3hB
3�B
4B
4B
4nB
4�B
4�B
5B
5ZB
5?B
5�B
5�B
5�B
5�B
5�B
6+B
6B
6FB
6zB
6�B
7B
72B
7LB
7�B
7�B
7�B
7�B
7�B
8B
88B
8B
9	B
9�B
9�B
9�B
:B
:^B
:DB
:^B
:DB
:�B
;JB
;0B
;B
;�B
;�B
<B
=B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?}B
?�B
?�B
?�B
?�B
@ B
@ B
@B
@B
@OB
@iB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
AB
@�B
A;B
A�B
AoB
A�B
B'B
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CGB
C�B
C�B
D3B
DMB
D�B
E9B
E�B
F�B
G�B
HB
G�B
G+B
F�B
G�B
F�B
F�B
F�B
F�B
GB
G�B
HfB
H�B
H�B
HfB
J#B
J�B
KB
K^B
KxB
J�B
J�B
J�B
K�B
L0B
LdB
L�B
M6B
MPB
MPB
M�B
N"B
NpB
NpB
N�B
N�B
N�B
OB
O(B
OBB
OvB
OvB
PB
P.B
P�B
P�B
QB
Q�B
Q�B
RB
RB
R�B
R�B
R:B
R B
R B
R:B
R:B
RTB
R�B
SB
S&B
SB
S[B
S�B
S�B
S�B
TFB
U2B
UMB
U�B
U�B
U�B
VB
VB
VSB
V�B
VmB
V�B
V9B
U�B
UB
T�B
UMB
T�B
T{B
TFB
S�B
T,B
TB
TB
T,B
T�B
T�B
T�B
U�B
U�B
U�B
V9B
VB
VSB
V�B
WsB
W�B
XB
XyB
X�B
YB
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
[WB
\�B
^B
^�B
_�B
_�B
_pB
`�B
`�B
aHB
a�B
a�B
a�B
a�B
a�B
bNB
b�B
bhB
bNB
b�B
bhB
bhB
b4B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
bhB
bNB
bhB
bNB
bhB
b�B
cB
cTB
c:B
c�B
c�B
dB
d�B
d�B
ezB
e`B
ezB
e�B
e�B
ffB
ffB
f�B
f�B
f�B
gB
g8B
g�B
g�B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
iyB
i�B
j0B
j0B
jKB
jeB
j�B
k6B
k6B
k6B
k�B
k�B
k�B
k�B
l=B
l"B
l=B
l=B
l�B
l�B
l�B
m)B
m]B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
nB
nIB
n�B
o B
oB
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
qB
p�B
qAB
qAB
qAB
q�B
rB
r-B
raB
r�B
r�B
sB
shB
shB
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
uB
u%B
u%B
u�B
u�B
v+B
vFB
vFB
vFB
vzB
vzB
v�B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xlB
xRB
x�B
x�B
y$B
y$B
yrB
y�B
y�B
z*B
zDB
{J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104900  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172959  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172959  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172959                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023007  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023007  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                