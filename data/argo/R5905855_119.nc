CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:32:09Z creation;2022-06-04T19:32:10Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604193209  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�Ў
m�51   @�Ўl�d @/@     �c�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�33B���B�  B���B���B�  B�33B���B���B�  B���B�  B虚B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB33CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @G�@n{@�
=@�
=A�A;�A[�A{�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBN�HBV�HB^�HBf�HBn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B��
B�=qB���B�=qB�p�B�=qB�=qB�p�Bϣ�B�=qB�=qB�p�B�=qB�p�B�
>B�=qB�p�B�p�B�p�B�p�B�p�C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=��C?�RCA�CC��CE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C���C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
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
Dۺ=D��
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
D�3�D�w
D��
D��
D�7
D�w
D��
D��
D�7
D�w
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ĜA���A϶FAϳ�Aϳ3AϹ�A�]/A�MA�'�A���A��5A��2A�ѷA��<A��#A��mA��gA���A��UAο}AξwAνAλ0AθAηLAη�Aη�Aγ�AβaAα'AίOAέCAή}AίAί�Aί�AΰUAί�Aή�AέCAΪ�AΨXA�oiA�`BA��A��A��5A��A�P�A�4nA��fA�c�A�V�A��zA�E9A���A���A��A��A���A�0UA�N�A��tA�A�A���A���A�5A� iA�FA�A�FA��9A�@�A�+�A��A���A|��A{1Ay�AAvB�At6zAq�Ao�|Al��Ai�Af��Ad��Aa�HA\q�AW�AAR�AL�AFRTA@�A;YKA9�A7�A6c�A5�gA5�zA4��A1�A0�A/��A//�A.L0A,�AA*�A*VA*A)�A)5�A'��A&�jA%�MA%Q�A%6A$�A#�A#`�A"�]A!��A�	Ac�A�AߤA�A \A0�ARTA  A�zA��Aa|A	A��Ar�A��A��A��A�A��A@�A~�AQ�Aw2A�>AVAR�A��A�MA	�A��A�ZA��A�VA��A�|AZ�AخA� A��A�nA
��A	��A	t�A	�A	�A|A�XAtTA��A;�A��A��A͟A��A}�A7�A+A�~A�HA�zAz�A�A�A�DA9XAqA��A�A<6A�A��A!�A`�A u�A �@��^@�S@�PH@�G@��}@�c�@��/@�B[@���@�;d@�%@���@��0@�@��@���@�^�@�2a@�/�@���@��v@��@��@�+@�p;@���@�Q�@�@���@�&�@���@�u�@�$@�/�@��@��[@�(�@��@�v`@��@�j@”@���@�@@���@��@���@���@߾w@�f�@��@�j@�n/@��2@܂A@���@���@ۛ=@�c@�v`@�C@�	@���@٥@��@�D�@�{@׺^@�D�@Փ�@ՍP@�Dg@��@ԭ�@Ԅ�@�9X@ӯ�@Ӕ�@�w2@��@ҋD@҈�@҃@�Ta@��@њk@�qv@�E9@�Y@���@�b@�S&@�ں@�|�@�1�@��@�	@��.@��}@͌~@�e,@���@�h�@�V�@�M�@�:�@��@���@˧�@�T�@ʶ�@�V�@�C-@�e@��K@���@�YK@�(�@��@��;@�e�@�+@�M@���@Ē�@ĂA@�p;@�6@Ò:@�]�@�Q�@�O�@�Vm@�Mj@��@ªe@�7�@��@���@�@�a@�@@��m@�g8@�"h@��@��D@���@��@���@�!@���@��V@�C@�kQ@���@���@���@��o@�d�@��'@�S@�z�@�	�@��@�|@�<6@�6z@�(�@��@�@@��z@�ϫ@��S@�E9@��@���@���@��@�K^@���@���@�c�@�=�@���@�4@���@�Y@�L0@���@�˒@�e,@���@�w�@�>B@�H@��b@��9@�W�@�$@���@��m@��@�!�@���@�v`@�L�@��@��@��b@�H�@�e@��@���@��@�~(@�	@���@��h@�a�@���@���@�ff@��@��:@�Dg@�&@�ߤ@���@�u%@�0U@��@���@���@� \@���@��@�g8@�7�@�-�@�O@��o@��@��5@��m@��.@�bN@�:*@��Z@���@� \@�s�@�#:@���@�RT@�+@�!-@��@���@��p@�oi@�2�@���@��z@��'@���@�~�@�B�@��@�͟@���@�s�@�L0@���@��@���@��q@�L�@�ی@��@�R�@��@��K@��P@�F�@��@���@�~(@�l"@�<�@��D@��H@��'@���@�S�@���@��@��@��@��M@�[W@��@��@��b@�@��j@���@�c�@�0�@��@��@�*�@��@��3@���@�&�@���@���@���@�oi@�R�@�0U@��A@���@��{@�}�@�[W@���@��@���@�j@�)_@��@���@���@�r�@��W@��$@�t�@�P�@�'�@��@��B@��.@�y>@�c�@�(�@��a@��'@���@�L�@�.I@�V@��_@�M�@�;�@��@��	@�H�@�.I@��@��O@���@�D�@��o@��[@���@��f@�]�@�@O@�#�@�
=@��@���@�z@�!@��@�zx@�a�@�:�@��@��@���@��]@�u�@�4@��@X�@(@~��@~d�@~W�@~E�@}��@}��@|��@|Ɇ@|�@|S�@{�@@z��@z�A@zv�@z3�@z@y�@y��@x�?@xM@xb@xG@w��@w��@we�@w4�@v��@v��@v}V@v3�@v{@u�@u=�@t��@tS�@t@s��@s��@s�@s�@s�f@so@r�h@r�@r�@rp;@rM�@q��@q�-@qzx@qN<@q�@p�@p@o��@o�@na|@n-@m�h@l��@k�]@k��@kX�@j��@j$�@iԕ@i��@i=�@hɆ@g�A@g.I@fxl@e�t@ew2@d��@c�@c)_@b��@bQ@a�@ao @aS&@a*0@`��@`��@_�&@_A�@^�<@^R�@]�o@]�7@]rG@]Q�@\��@\�j@\S�@\�@[�{@['�@Z�@Z�B@Z��@Zi�@ZJ@Y��@Y�h@Yc�@YS&@Y:�@X�@XPH@W�@Wv`@V�y@VJ�@U�@U�@T��@Tj@TU2@TD�@T!@S��@S��@S��@S��@SMj@R�M@R�\@Ru%@R	@QX@Q*0@Q�@P�E@P�D@P`�@P@O�}@O�4@N�@N	@M��@MQ�@L�@L��@L6@K�V@J�@J�!@J��@J��@JTa@I�@I&�@H�/@H��@Hh�@HQ�@H<�@H%�@H  @G��@Ga@G�@F��@Fu%@F0U@E��@E��@E�'@E(�@D��@D�@Dѷ@D��@D�.@DXy@D(�@D@C9�@Bں@B�R@Bxl@B=q@A�@A�@@�.@@oi@@Ft@?�@?��@?��@?X�@?@>l�@>�@>e@=�@=�@<�u@<'R@;�@:��@9�@9=�@9�@8��@8�`@8��@8Xy@7��@7@7 i@6�@6�2@6�s@6�<@6q�@6W�@5�>@5��@5��@5�@5@@4Ɇ@4�I@4`�@4:�@41'@3��@3��@3>�@2��@2ں@2҉@2��@2�@1��@1x�@1T�@0��@0�4@0g8@0Xy@01'@0@/�+@/˒@/]�@/4�@/�@.��@.��@.�<@.�h@.�@.J�@-�S@,Ɇ@,~(@+��@+�*@+�P@+J#@*�c@*��@*�\@*h
@*R�@*�@)�@)��@)�t@)^�@)?}@):�@)7L@)&�@)�@(�v@(~(@(D�@'��@'U�@&�H@&GE@&M�@&R�@&J�@&.�@%��@%u�@$�@$�@$��@$w�@$6@$�@#�r@#�6@#�$@#x@#U�@#P�@#P�@#@O@#�@"��@"��@"u%@"n�@"Ta@"@�@"J@ ��@ M@ �@�@��@O@!-@�@��@ں@ȴ@��@�+@H�@�.@��@�@��@��@�@|@s�@a�@A @+�@q@V@�p@��@j@Xy@A�@�@��@o�@4�@ں@}V@&�@�@O@u@��@|@G�@�@V@�`@��@7�@�}@Y@��@��@_�@Ov@J�@E�@$�@ �@�>@��@�n@�h@u�@+�@��@tT@~@��@��@�4@g�@_p@H�@�@��@��@��@z@h
@1�@�Z@��@^�@֡@�@7�@��@��@�k@~�@_p@RT@=@�@�"@�c@�y@�@�@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ĜA���A϶FAϳ�Aϳ3AϹ�A�]/A�MA�'�A���A��5A��2A�ѷA��<A��#A��mA��gA���A��UAο}AξwAνAλ0AθAηLAη�Aη�Aγ�AβaAα'AίOAέCAή}AίAί�Aί�AΰUAί�Aή�AέCAΪ�AΨXA�oiA�`BA��A��A��5A��A�P�A�4nA��fA�c�A�V�A��zA�E9A���A���A��A��A���A�0UA�N�A��tA�A�A���A���A�5A� iA�FA�A�FA��9A�@�A�+�A��A���A|��A{1Ay�AAvB�At6zAq�Ao�|Al��Ai�Af��Ad��Aa�HA\q�AW�AAR�AL�AFRTA@�A;YKA9�A7�A6c�A5�gA5�zA4��A1�A0�A/��A//�A.L0A,�AA*�A*VA*A)�A)5�A'��A&�jA%�MA%Q�A%6A$�A#�A#`�A"�]A!��A�	Ac�A�AߤA�A \A0�ARTA  A�zA��Aa|A	A��Ar�A��A��A��A�A��A@�A~�AQ�Aw2A�>AVAR�A��A�MA	�A��A�ZA��A�VA��A�|AZ�AخA� A��A�nA
��A	��A	t�A	�A	�A|A�XAtTA��A;�A��A��A͟A��A}�A7�A+A�~A�HA�zAz�A�A�A�DA9XAqA��A�A<6A�A��A!�A`�A u�A �@��^@�S@�PH@�G@��}@�c�@��/@�B[@���@�;d@�%@���@��0@�@��@���@�^�@�2a@�/�@���@��v@��@��@�+@�p;@���@�Q�@�@���@�&�@���@�u�@�$@�/�@��@��[@�(�@��@�v`@��@�j@”@���@�@@���@��@���@���@߾w@�f�@��@�j@�n/@��2@܂A@���@���@ۛ=@�c@�v`@�C@�	@���@٥@��@�D�@�{@׺^@�D�@Փ�@ՍP@�Dg@��@ԭ�@Ԅ�@�9X@ӯ�@Ӕ�@�w2@��@ҋD@҈�@҃@�Ta@��@њk@�qv@�E9@�Y@���@�b@�S&@�ں@�|�@�1�@��@�	@��.@��}@͌~@�e,@���@�h�@�V�@�M�@�:�@��@���@˧�@�T�@ʶ�@�V�@�C-@�e@��K@���@�YK@�(�@��@��;@�e�@�+@�M@���@Ē�@ĂA@�p;@�6@Ò:@�]�@�Q�@�O�@�Vm@�Mj@��@ªe@�7�@��@���@�@�a@�@@��m@�g8@�"h@��@��D@���@��@���@�!@���@��V@�C@�kQ@���@���@���@��o@�d�@��'@�S@�z�@�	�@��@�|@�<6@�6z@�(�@��@�@@��z@�ϫ@��S@�E9@��@���@���@��@�K^@���@���@�c�@�=�@���@�4@���@�Y@�L0@���@�˒@�e,@���@�w�@�>B@�H@��b@��9@�W�@�$@���@��m@��@�!�@���@�v`@�L�@��@��@��b@�H�@�e@��@���@��@�~(@�	@���@��h@�a�@���@���@�ff@��@��:@�Dg@�&@�ߤ@���@�u%@�0U@��@���@���@� \@���@��@�g8@�7�@�-�@�O@��o@��@��5@��m@��.@�bN@�:*@��Z@���@� \@�s�@�#:@���@�RT@�+@�!-@��@���@��p@�oi@�2�@���@��z@��'@���@�~�@�B�@��@�͟@���@�s�@�L0@���@��@���@��q@�L�@�ی@��@�R�@��@��K@��P@�F�@��@���@�~(@�l"@�<�@��D@��H@��'@���@�S�@���@��@��@��@��M@�[W@��@��@��b@�@��j@���@�c�@�0�@��@��@�*�@��@��3@���@�&�@���@���@���@�oi@�R�@�0U@��A@���@��{@�}�@�[W@���@��@���@�j@�)_@��@���@���@�r�@��W@��$@�t�@�P�@�'�@��@��B@��.@�y>@�c�@�(�@��a@��'@���@�L�@�.I@�V@��_@�M�@�;�@��@��	@�H�@�.I@��@��O@���@�D�@��o@��[@���@��f@�]�@�@O@�#�@�
=@��@���@�z@�!@��@�zx@�a�@�:�@��@��@���@��]@�u�@�4@��@X�@(@~��@~d�@~W�@~E�@}��@}��@|��@|Ɇ@|�@|S�@{�@@z��@z�A@zv�@z3�@z@y�@y��@x�?@xM@xb@xG@w��@w��@we�@w4�@v��@v��@v}V@v3�@v{@u�@u=�@t��@tS�@t@s��@s��@s�@s�@s�f@so@r�h@r�@r�@rp;@rM�@q��@q�-@qzx@qN<@q�@p�@p@o��@o�@na|@n-@m�h@l��@k�]@k��@kX�@j��@j$�@iԕ@i��@i=�@hɆ@g�A@g.I@fxl@e�t@ew2@d��@c�@c)_@b��@bQ@a�@ao @aS&@a*0@`��@`��@_�&@_A�@^�<@^R�@]�o@]�7@]rG@]Q�@\��@\�j@\S�@\�@[�{@['�@Z�@Z�B@Z��@Zi�@ZJ@Y��@Y�h@Yc�@YS&@Y:�@X�@XPH@W�@Wv`@V�y@VJ�@U�@U�@T��@Tj@TU2@TD�@T!@S��@S��@S��@S��@SMj@R�M@R�\@Ru%@R	@QX@Q*0@Q�@P�E@P�D@P`�@P@O�}@O�4@N�@N	@M��@MQ�@L�@L��@L6@K�V@J�@J�!@J��@J��@JTa@I�@I&�@H�/@H��@Hh�@HQ�@H<�@H%�@H  @G��@Ga@G�@F��@Fu%@F0U@E��@E��@E�'@E(�@D��@D�@Dѷ@D��@D�.@DXy@D(�@D@C9�@Bں@B�R@Bxl@B=q@A�@A�@@�.@@oi@@Ft@?�@?��@?��@?X�@?@>l�@>�@>e@=�@=�@<�u@<'R@;�@:��@9�@9=�@9�@8��@8�`@8��@8Xy@7��@7@7 i@6�@6�2@6�s@6�<@6q�@6W�@5�>@5��@5��@5�@5@@4Ɇ@4�I@4`�@4:�@41'@3��@3��@3>�@2��@2ں@2҉@2��@2�@1��@1x�@1T�@0��@0�4@0g8@0Xy@01'@0@/�+@/˒@/]�@/4�@/�@.��@.��@.�<@.�h@.�@.J�@-�S@,Ɇ@,~(@+��@+�*@+�P@+J#@*�c@*��@*�\@*h
@*R�@*�@)�@)��@)�t@)^�@)?}@):�@)7L@)&�@)�@(�v@(~(@(D�@'��@'U�@&�H@&GE@&M�@&R�@&J�@&.�@%��@%u�@$�@$�@$��@$w�@$6@$�@#�r@#�6@#�$@#x@#U�@#P�@#P�@#@O@#�@"��@"��@"u%@"n�@"Ta@"@�@"J@ ��@ M@ �@�@��@O@!-@�@��@ں@ȴ@��@�+@H�@�.@��@�@��@��@�@|@s�@a�@A @+�@q@V@�p@��@j@Xy@A�@�@��@o�@4�@ں@}V@&�@�@O@u@��@|@G�@�@V@�`@��@7�@�}@Y@��@��@_�@Ov@J�@E�@$�@ �@�>@��@�n@�h@u�@+�@��@tT@~@��@��@�4@g�@_p@H�@�@��@��@��@z@h
@1�@�Z@��@^�@֡@�@7�@��@��@�k@~�@_p@RT@=@�@�"@�c@�y@�@�@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�xB	�DB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�2B	�2B	�LB	�LB	�2B	�2B	�2B	�B	�B	��B	��B	��B	�B	�LB	�fB	�B	��B	��B	��B	��B	��B	�LB	�LB	�fB	�fB	�fB	�fB	�fB	�LB	�LB	��B	��B	�0B	�RB
9B
?�B
J#B
]IB
� B
��B
��B
� B
�B
�lB
�cB
��B
��B
��B
�HB
��B
��B
��B
��B
�4B
��B
�{B
zDB
jeB
H�B
H�B
G�B
=�B
/ B
{B	�GB	�B	�?B	�B	��B	��B	��B	��B	u�B	gRB	[�B	O�B	>�B	,B	4B��B�sB�7B�-B��B��B��B�B�OB�IB�vB��B�iB��B�|B�xB	AB	tB	B		B	<B	:B	B	�B	6`B	DMB	E�B	BuB	HKB	J#B	`�B	[=B	V�B	XyB	_VB	g�B	j�B	shB	�B	�fB	��B	��B	��B	�B	��B	�B	��B	��B	��B	~(B	}�B	~B	}<B	~�B	��B	��B	�B	ȀB	̈́B	��B	� B	ΊB	ʦB	ĶB	��B	�XB	��B	�B	�bB	�.B	��B	�?B	��B	�9B	��B	�B	�VB	�hB	�pB	�B	�:B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�jB	͟B	͹B	�B	��B	�B	�vB	��B	� B	ңB	��B	ۦB	ܬB	�#B	�B	خB	�yB	ٚB	�kB	�jB	ޞB	�B	ޞB	ݲB	�B	�:B	�B	�qB	��B	�VB	�B	�BB	�cB
�B
aB
�B
�B
�B
�B
 �B
 B
 �B	��B	�}B	�wB	��B	�VB
 iB
�B
B
;B	��B	�}B	�0B	��B	��B	��B	�NB	��B	�B	�gB	�FB	�FB	�gB	ԯB	�@B	�4B	��B	�.B	�.B	�BB	��B	�\B	�(B	�BB	�BB	��B	�.B	��B	�B	��B	�B	�sB	�YB	��B	�&B	ҽB	�oB	� B	��B	ѷB	� B	��B	уB	�NB	�oB	ԕB	�B	�MB	ևB	�yB	�KB	�7B	�xB	ބB	�B	��B	�:B	�&B	��B	�B	�B	�B	�tB	�@B	�B	�ZB	��B	�B	�8B	�8B	�B	�B	��B	��B	��B	�0B	�B	�0B	�B	�B	��B	��B	�=B	�B	�wB	�B	��B	�[B	�B	�B	�B	�[B	��B	�B	��B	�B	��B	��B	�RB	��B	�dB	�PB	�B	�B	�B	�B	�jB	�PB	�jB	��B	�jB	�PB	��B	��B	��B	��B	��B	��B	�B	�.B
 �B
 �B
 �B
 �B
 iB
UB
;B
�B
B
uB
�B
�B
�B
�B
�B
�B
�B
%B
YB
+B
+B
+B
EB
B
�B
�B
�B
�B
YB
�B
_B
�B
KB
�B
	RB

XB
^B
B
	�B
	�B
DB
�B
�B
B
vB
(B
�B
"B
�B
�B
�B
�B
"B
VB
pB
�B
�B
�B
"B
B
�B
�B
�B
�B
�B
�B
NB
B
�B
�B
�B
�B
FB
�B
�B
B
�B
mB
SB
?B
YB
�B
�B
B
EB
_B
EB
�B
_B
�B
�B
�B
�B
�B
eB
eB
QB
kB
B
	B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
�B
B
]B
�B
IB
~B
IB
�B
�B
�B
~B
�B
B
�B
�B
pB
�B
�B
 BB
 \B
 �B
 �B
 �B
 �B
!-B
!B
!�B
!�B
!�B
!�B
"�B
"�B
#nB
#�B
#�B
$B
$ZB
$tB
$�B
%,B
%`B
%�B
%�B
%�B
%�B
&B
&�B
'RB
'�B
(
B
(>B
(�B
)B
)yB
)�B
)�B
)�B
)�B
*KB
*�B
*�B
*�B
+kB
+kB
+�B
,"B
,�B
,�B
,�B
,�B
,�B
,�B
-B
,�B
-)B
-wB
.IB
.IB
.�B
.�B
.�B
.�B
/B
/5B
/�B
0!B
0B
/�B
0;B
0�B
1'B
1B
1�B
1'B
0�B
0�B
0�B
0�B
1vB
2�B
2�B
2�B
3B
3hB
3�B
3�B
4B
4�B
4�B
4�B
5?B
5%B
5�B
5�B
5�B
5�B
6+B
6B
6FB
5�B
6FB
6�B
6�B
7B
7�B
7�B
7�B
7�B
8B
8RB
8�B
8�B
8�B
8�B
9rB
:B
:B
9�B
:DB
:*B
:DB
:*B
:�B
:�B
;B
;B
:�B
;0B
;JB
;dB
;�B
;�B
<B
<jB
<6B
<�B
<�B
<�B
=VB
=VB
=VB
=<B
=<B
=qB
=�B
=�B
>(B
>BB
>BB
>BB
>BB
>wB
>�B
>�B
>�B
>]B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
AUB
BB
A�B
B[B
B�B
C-B
CaB
CaB
C�B
C-B
CGB
CaB
C�B
D�B
ESB
FB
FB
F?B
E�B
ESB
E�B
F%B
F?B
F?B
FtB
GEB
G�B
H1B
HKB
HfB
HKB
H�B
H�B
H�B
IB
H�B
IlB
I�B
I�B
JrB
J�B
J�B
J�B
J�B
K)B
KDB
K^B
KxB
K^B
K^B
K�B
K�B
LJB
LJB
L�B
L�B
MB
MB
N<B
NVB
NVB
NpB
N�B
N�B
N�B
N�B
N�B
O(B
OBB
O�B
PB
QB
QhB
Q4B
QB
QNB
Q4B
Q4B
QB
QB
Q4B
Q�B
R:B
RTB
RTB
RoB
RoB
RoB
SB
S[B
SuB
SuB
S[B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U�B
VB
VB
VSB
V�B
V�B
V�B
WYB
W?B
WYB
WYB
WYB
WsB
W�B
W�B
WsB
XB
XB
X+B
XEB
X_B
X�B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
ZQB
[	B
[	B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
^B
^B
^B
^B
^jB
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`BB
`�B
a-B
a-B
aB
a-B
abB
a�B
a�B
bB
bNB
b�B
b�B
cnB
c�B
c�B
c�B
d&B
d@B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gmB
h
B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
iB
i*B
iB
i_B
iyB
i�B
i�B
i�B
j0B
jB
jB
j0B
jB
j0B
jB
jB
k6B
kkB
l"B
lqB
lWB
l=B
l=B
lWB
l�B
l�B
m]B
mwB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n}B
ncB
n/B
n�B
n�B
n�B
n�B
o B
o B
o B
oB
oB
oB
o5B
o5B
oOB
o�B
o�B
p!B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
qB
q'B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
sMB
s�B
tB
s�B
s�B
s�B
t9B
tB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
u%B
t�B
u%B
utB
utB
u�B
u�B
vFB
vFB
v`B
v�B
v�B
vzB
vFB
vFB
v�B
v�B
w2B
wfB
w�B
xB
xB
xB
xB
xRB
x�B
x�B
x�B
y>B
yXB
y�B
zB
zDB
z�B
{B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	�xB	�DB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�2B	�2B	�LB	�LB	�2B	�2B	�2B	�B	�B	��B	��B	��B	�B	�LB	�fB	�B	��B	��B	��B	��B	��B	�LB	�LB	�fB	�fB	�fB	�fB	�fB	�LB	�LB	��B	��B	�0B	�RB
9B
?�B
J#B
]IB
� B
��B
��B
� B
�B
�lB
�cB
��B
��B
��B
�HB
��B
��B
��B
��B
�4B
��B
�{B
zDB
jeB
H�B
H�B
G�B
=�B
/ B
{B	�GB	�B	�?B	�B	��B	��B	��B	��B	u�B	gRB	[�B	O�B	>�B	,B	4B��B�sB�7B�-B��B��B��B�B�OB�IB�vB��B�iB��B�|B�xB	AB	tB	B		B	<B	:B	B	�B	6`B	DMB	E�B	BuB	HKB	J#B	`�B	[=B	V�B	XyB	_VB	g�B	j�B	shB	�B	�fB	��B	��B	��B	�B	��B	�B	��B	��B	��B	~(B	}�B	~B	}<B	~�B	��B	��B	�B	ȀB	̈́B	��B	� B	ΊB	ʦB	ĶB	��B	�XB	��B	�B	�bB	�.B	��B	�?B	��B	�9B	��B	�B	�VB	�hB	�pB	�B	�:B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�jB	͟B	͹B	�B	��B	�B	�vB	��B	� B	ңB	��B	ۦB	ܬB	�#B	�B	خB	�yB	ٚB	�kB	�jB	ޞB	�B	ޞB	ݲB	�B	�:B	�B	�qB	��B	�VB	�B	�BB	�cB
�B
aB
�B
�B
�B
�B
 �B
 B
 �B	��B	�}B	�wB	��B	�VB
 iB
�B
B
;B	��B	�}B	�0B	��B	��B	��B	�NB	��B	�B	�gB	�FB	�FB	�gB	ԯB	�@B	�4B	��B	�.B	�.B	�BB	��B	�\B	�(B	�BB	�BB	��B	�.B	��B	�B	��B	�B	�sB	�YB	��B	�&B	ҽB	�oB	� B	��B	ѷB	� B	��B	уB	�NB	�oB	ԕB	�B	�MB	ևB	�yB	�KB	�7B	�xB	ބB	�B	��B	�:B	�&B	��B	�B	�B	�B	�tB	�@B	�B	�ZB	��B	�B	�8B	�8B	�B	�B	��B	��B	��B	�0B	�B	�0B	�B	�B	��B	��B	�=B	�B	�wB	�B	��B	�[B	�B	�B	�B	�[B	��B	�B	��B	�B	��B	��B	�RB	��B	�dB	�PB	�B	�B	�B	�B	�jB	�PB	�jB	��B	�jB	�PB	��B	��B	��B	��B	��B	��B	�B	�.B
 �B
 �B
 �B
 �B
 iB
UB
;B
�B
B
uB
�B
�B
�B
�B
�B
�B
�B
%B
YB
+B
+B
+B
EB
B
�B
�B
�B
�B
YB
�B
_B
�B
KB
�B
	RB

XB
^B
B
	�B
	�B
DB
�B
�B
B
vB
(B
�B
"B
�B
�B
�B
�B
"B
VB
pB
�B
�B
�B
"B
B
�B
�B
�B
�B
�B
�B
NB
B
�B
�B
�B
�B
FB
�B
�B
B
�B
mB
SB
?B
YB
�B
�B
B
EB
_B
EB
�B
_B
�B
�B
�B
�B
�B
eB
eB
QB
kB
B
	B
�B
�B
�B
�B
�B
#B
WB
�B
�B
�B
�B
B
]B
�B
IB
~B
IB
�B
�B
�B
~B
�B
B
�B
�B
pB
�B
�B
 BB
 \B
 �B
 �B
 �B
 �B
!-B
!B
!�B
!�B
!�B
!�B
"�B
"�B
#nB
#�B
#�B
$B
$ZB
$tB
$�B
%,B
%`B
%�B
%�B
%�B
%�B
&B
&�B
'RB
'�B
(
B
(>B
(�B
)B
)yB
)�B
)�B
)�B
)�B
*KB
*�B
*�B
*�B
+kB
+kB
+�B
,"B
,�B
,�B
,�B
,�B
,�B
,�B
-B
,�B
-)B
-wB
.IB
.IB
.�B
.�B
.�B
.�B
/B
/5B
/�B
0!B
0B
/�B
0;B
0�B
1'B
1B
1�B
1'B
0�B
0�B
0�B
0�B
1vB
2�B
2�B
2�B
3B
3hB
3�B
3�B
4B
4�B
4�B
4�B
5?B
5%B
5�B
5�B
5�B
5�B
6+B
6B
6FB
5�B
6FB
6�B
6�B
7B
7�B
7�B
7�B
7�B
8B
8RB
8�B
8�B
8�B
8�B
9rB
:B
:B
9�B
:DB
:*B
:DB
:*B
:�B
:�B
;B
;B
:�B
;0B
;JB
;dB
;�B
;�B
<B
<jB
<6B
<�B
<�B
<�B
=VB
=VB
=VB
=<B
=<B
=qB
=�B
=�B
>(B
>BB
>BB
>BB
>BB
>wB
>�B
>�B
>�B
>]B
>�B
>�B
?�B
@�B
@�B
@�B
@�B
AUB
BB
A�B
B[B
B�B
C-B
CaB
CaB
C�B
C-B
CGB
CaB
C�B
D�B
ESB
FB
FB
F?B
E�B
ESB
E�B
F%B
F?B
F?B
FtB
GEB
G�B
H1B
HKB
HfB
HKB
H�B
H�B
H�B
IB
H�B
IlB
I�B
I�B
JrB
J�B
J�B
J�B
J�B
K)B
KDB
K^B
KxB
K^B
K^B
K�B
K�B
LJB
LJB
L�B
L�B
MB
MB
N<B
NVB
NVB
NpB
N�B
N�B
N�B
N�B
N�B
O(B
OBB
O�B
PB
QB
QhB
Q4B
QB
QNB
Q4B
Q4B
QB
QB
Q4B
Q�B
R:B
RTB
RTB
RoB
RoB
RoB
SB
S[B
SuB
SuB
S[B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
UMB
U�B
VB
VB
VSB
V�B
V�B
V�B
WYB
W?B
WYB
WYB
WYB
WsB
W�B
W�B
WsB
XB
XB
X+B
XEB
X_B
X�B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
ZQB
[	B
[	B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
^B
^B
^B
^B
^jB
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`BB
`�B
a-B
a-B
aB
a-B
abB
a�B
a�B
bB
bNB
b�B
b�B
cnB
c�B
c�B
c�B
d&B
d@B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gmB
h
B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
iB
i*B
iB
i_B
iyB
i�B
i�B
i�B
j0B
jB
jB
j0B
jB
j0B
jB
jB
k6B
kkB
l"B
lqB
lWB
l=B
l=B
lWB
l�B
l�B
m]B
mwB
mwB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n}B
ncB
n/B
n�B
n�B
n�B
n�B
o B
o B
o B
oB
oB
oB
o5B
o5B
oOB
o�B
o�B
p!B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
qB
q'B
q'B
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
sMB
s�B
tB
s�B
s�B
s�B
t9B
tB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
u%B
t�B
u%B
utB
utB
u�B
u�B
vFB
vFB
v`B
v�B
v�B
vzB
vFB
vFB
v�B
v�B
w2B
wfB
w�B
xB
xB
xB
xB
xRB
x�B
x�B
x�B
y>B
yXB
y�B
zB
zDB
z�B
{B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105253  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193209  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193210  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193210                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043217  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043217  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                