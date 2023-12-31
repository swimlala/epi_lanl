CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-28T12:00:50Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߜ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20220228120050  20220228120050  4903322 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               EA   AO  8286                            2B  A   NAVIS_A                         1165                            170425                          863 @ٽ�'D1   @ٽ33A�@9���S���d��1'1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         EA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�|�D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A�A;�A[�A{�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBN�HBV�HB^�HBf�HBn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
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
D�s�D��
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
D۷
D��
D�7
D�w
Dܷ
D��
D�7
D�s�Dݷ
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
D�z=D�
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
D�=pD�Z=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�bNA�^5A�bNA�bNA�dZA�ffA�ffA�ffA�jA�jA�jA�jA�n�A�n�A�G�A�JA��`A� �A��A�-A���A��A���A���A�33A���A�33A�bA��A��-A�\)A��A��A� �A�{A� �A�1A�
=A�VA�VA�JA���A���A��jA���A�p�A���A��7A�Q�A�JA���A�jA�A�\)A�  A�7LA�^5A�=qA��FA��A�~�A���A�1A��A�hsA���A�dZA���A��`A�M�A�1'A���A��jA�hsA�;dA���A�bA�^5A���A���A���A�I�A�ƨA�r�A�(�A��/A�1A���A�
=A��uA��9A���A�  A��PA�(�A�{A��A�p�A�{A���A�  A�VA�
=A~�\A|�A{G�Ay�;Axv�Av��At�\As�hAr�/ArAq�-Aq|�Ap  AnQ�Al��Aj��AidZAh��AgO�Af9XAex�AeG�Ad(�Ac?}Abz�A`�RA]��A[�PA[dZAZz�AX�AV��AT�AT��ATE�AS\)ARĜAQ�#AQAQ�AQ��AO7LAL��AK?}AK
=AJ�9AJQ�AJ�AI�PAI+AHĜAG�AF��AFM�AE��AE��AD�HAC33AB-AA��A@��A?oA=��A<��A;G�A:^5A8��A7�A6��A5��A4�A3�A2��A1�
A0E�A/�7A.jA,�yA,$�A*�A*9XA*(�A*�A)�A)A(��A(=qA'hsA&v�A& �A$��A$E�A#�A" �A!�A!��A!��A!�FA!XA �!A 1'AƨA��A33A�/AA�At�A�HA(�A��A�A��A�/AM�A33AjA�#A33AoA�uA1A�hA"�AjA��A;dA�;A�
AG�AE�A`BA
��A	7LAv�AM�A�
Ap�AK�A&�A�RA�A��AjA�hA M�@�-@�X@���@��R@��/@�Q�@���@��@���@�(�@�w@�|�@�33@�&�@@�;d@�t�@��@睲@睲@�dZ@�^@�Q�@�+@�7L@�t�@��@�  @�ȴ@�r�@�  @ׅ@�@�ff@��/@�1@�+@���@ҧ�@Ұ!@ҧ�@�-@��@Л�@Ѓ@�r�@�Z@��@�ȴ@�M�@͡�@̴9@˝�@�r�@�X@�V@���@ă@å�@��H@�~�@��@��h@�Ĝ@��m@�v�@�&�@�bN@���@�M�@�@�%@�1'@�ȴ@�=q@�@��9@�1@��@��@�33@�^5@�X@���@��@���@��D@��w@�=q@�?}@��`@���@���@��@���@��@�K�@�K�@�;d@��@�ȴ@�E�@���@��@��D@�ƨ@��@��^@�Ĝ@�I�@��P@�o@��@���@��+@�^5@�M�@�$�@�@��@�Ĝ@��@�o@���@��-@�?}@�(�@��@�n�@�?}@�Z@��@�t�@��@���@���@��-@�?}@��@�Ĝ@��9@��@��9@��@���@�Z@�dZ@��@���@��7@�hs@�/@��@�j@�(�@��w@�C�@���@�{@���@�x�@�X@���@��j@�z�@�Z@�9X@�(�@��@��@�1@�1@�b@�b@�  @�S�@�;d@�
=@��@���@��R@��!@���@���@���@�~�@�v�@��#@��h@�X@���@��`@�Ĝ@�j@�9X@�1'@�1'@�1@��@�|�@�|�@��y@���@���@��!@���@���@��\@�~�@�n�@�M�@�=q@��@���@��@��D@�(�@�1@���@��@��;@��
@���@��w@��@���@��P@�t�@�33@���@��\@�5?@��@���@��h@�/@��@�V@���@��@�9X@�(�@� �@��@�b@�  @�@�@~��@~�+@~E�@}��@}�@}�@}`B@}/@|�j@|I�@|�@{�m@{33@z�!@zn�@z=q@y�@y��@y7L@x��@x�`@x��@x�9@xr�@xA�@x1'@x  @w��@v�y@u@up�@u�@t�D@t(�@st�@s33@s@r�\@rn�@rn�@r�@q��@q%@pbN@pA�@p �@o�@o�;@o��@n�+@m/@l�D@lz�@lj@l9X@kdZ@j��@j=q@j�@j�@jJ@jJ@j-@j-@j=q@j=q@j=q@j=q@j�@j�@j�@j-@j-@j�@j�@jJ@jJ@i��@i�@i�7@h��@g�;@f�+@f@d��@c�
@ct�@c@b��@b~�@bM�@a��@a��@ahs@a&�@`��@`Ĝ@`��@`bN@`b@`b@_�@_�@_�P@^�+@]V@\��@\�@\z�@\I�@\9X@\1@[��@[��@[�F@[dZ@[S�@[S�@[S�@[@Z~�@ZM�@Yx�@Y%@X�9@X �@Wl�@W
=@V�y@V�y@W�@W;d@W�P@Wl�@WK�@W�@V�@V�R@V�+@U�@S�F@S33@SC�@S"�@RM�@Q�@Q�#@Q�@P�`@PbN@P �@Pr�@PĜ@Pb@O�@O�w@O;d@O
=@N�R@Nv�@L��@Lz�@L(�@K�m@K��@KS�@K"�@J�@I�^@I7L@H��@H  @G�w@G��@G|�@Gl�@G+@F��@FV@E�-@E?}@E�@E�@D�@C�F@C�@Ct�@B�@Ahs@@r�@@b@?��@?l�@?�@>V@=�T@=��@=��@=@=@=�-@=��@=p�@=O�@=O�@=V@<�/@<9X@;t�@:�@:�!@:M�@9��@9��@9��@9�@8�`@8�@7�@7��@7�P@7\)@7K�@7+@7�@6�@6v�@6E�@6@5�@5V@4�/@4��@4��@4Z@4(�@4�@3�
@3�
@3ƨ@3t�@2��@2n�@2M�@2-@2�@1�#@0��@0A�@0 �@/�@/�;@/��@/�w@/�@/�@/��@/�P@/\)@.��@.$�@-@-O�@,�@,�@,�D@,Z@,(�@+��@+�
@+�F@+S�@+"�@*�@*��@*��@)�7@(��@(bN@(  @'�@'\)@'+@'�@'
=@&�@&��@&E�@%��@%/@$�j@$��@$�D@$z�@$j@$9X@$(�@#�m@#ƨ@#��@"�H@"�\@"=q@!�@!�#@!��@!��@!x�@!&�@ �`@ ��@ �u@ �@ r�@ Q�@��@��@�w@�P@�@��@�y@ȴ@��@5?@$�@$�@{@@{@@�@�T@�h@�@�@p�@`B@�@��@I�@9X@ƨ@33@��@^5@M�@=q@-@�#@G�@��@�`@Ĝ@�u@r�@1'@�@�P@\)@+@
=@�y@�R@v�@E�@@��@�-@?}@��@z�@Z@(�@��@��@t�@C�@33@33@"�@o@�@��@�!@n�@=q@��@hs@7L@&�@�@%@��@�`@��@�9@r�@1'@�;@��@�P@;d@�@��@ȴ@��@��@v�@ff@E�@$�@@�T@�-@�@�@�@`B@?}@�/@�j@�j@�@�D@j@9X@�@1@��@�
@��@t�@t�@t�@dZ@33@33@o@
��@
-@
�@
J@	�@	�#@	�^@	��@	�7@	x�@	x�@	G�@	7L@	�@	%@	%@��@�`@Ĝ@��@�@Q�@Q�@A�@1'@ �@�;@�P@l�@�@�R@�+@v�@ff@ff@V@ff@ff@ff@ff@@�-@��@�h@p�@`B@p�@O�@�@�@�@��@�@�@1@�
@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�dZA�bNA�^5A�bNA�bNA�dZA�ffA�ffA�ffA�jA�jA�jA�jA�n�A�n�A�G�A�JA��`A� �A��A�-A���A��A���A���A�33A���A�33A�bA��A��-A�\)A��A��A� �A�{A� �A�1A�
=A�VA�VA�JA���A���A��jA���A�p�A���A��7A�Q�A�JA���A�jA�A�\)A�  A�7LA�^5A�=qA��FA��A�~�A���A�1A��A�hsA���A�dZA���A��`A�M�A�1'A���A��jA�hsA�;dA���A�bA�^5A���A���A���A�I�A�ƨA�r�A�(�A��/A�1A���A�
=A��uA��9A���A�  A��PA�(�A�{A��A�p�A�{A���A�  A�VA�
=A~�\A|�A{G�Ay�;Axv�Av��At�\As�hAr�/ArAq�-Aq|�Ap  AnQ�Al��Aj��AidZAh��AgO�Af9XAex�AeG�Ad(�Ac?}Abz�A`�RA]��A[�PA[dZAZz�AX�AV��AT�AT��ATE�AS\)ARĜAQ�#AQAQ�AQ��AO7LAL��AK?}AK
=AJ�9AJQ�AJ�AI�PAI+AHĜAG�AF��AFM�AE��AE��AD�HAC33AB-AA��A@��A?oA=��A<��A;G�A:^5A8��A7�A6��A5��A4�A3�A2��A1�
A0E�A/�7A.jA,�yA,$�A*�A*9XA*(�A*�A)�A)A(��A(=qA'hsA&v�A& �A$��A$E�A#�A" �A!�A!��A!��A!�FA!XA �!A 1'AƨA��A33A�/AA�At�A�HA(�A��A�A��A�/AM�A33AjA�#A33AoA�uA1A�hA"�AjA��A;dA�;A�
AG�AE�A`BA
��A	7LAv�AM�A�
Ap�AK�A&�A�RA�A��AjA�hA M�@�-@�X@���@��R@��/@�Q�@���@��@���@�(�@�w@�|�@�33@�&�@@�;d@�t�@��@睲@睲@�dZ@�^@�Q�@�+@�7L@�t�@��@�  @�ȴ@�r�@�  @ׅ@�@�ff@��/@�1@�+@���@ҧ�@Ұ!@ҧ�@�-@��@Л�@Ѓ@�r�@�Z@��@�ȴ@�M�@͡�@̴9@˝�@�r�@�X@�V@���@ă@å�@��H@�~�@��@��h@�Ĝ@��m@�v�@�&�@�bN@���@�M�@�@�%@�1'@�ȴ@�=q@�@��9@�1@��@��@�33@�^5@�X@���@��@���@��D@��w@�=q@�?}@��`@���@���@��@���@��@�K�@�K�@�;d@��@�ȴ@�E�@���@��@��D@�ƨ@��@��^@�Ĝ@�I�@��P@�o@��@���@��+@�^5@�M�@�$�@�@��@�Ĝ@��@�o@���@��-@�?}@�(�@��@�n�@�?}@�Z@��@�t�@��@���@���@��-@�?}@��@�Ĝ@��9@��@��9@��@���@�Z@�dZ@��@���@��7@�hs@�/@��@�j@�(�@��w@�C�@���@�{@���@�x�@�X@���@��j@�z�@�Z@�9X@�(�@��@��@�1@�1@�b@�b@�  @�S�@�;d@�
=@��@���@��R@��!@���@���@���@�~�@�v�@��#@��h@�X@���@��`@�Ĝ@�j@�9X@�1'@�1'@�1@��@�|�@�|�@��y@���@���@��!@���@���@��\@�~�@�n�@�M�@�=q@��@���@��@��D@�(�@�1@���@��@��;@��
@���@��w@��@���@��P@�t�@�33@���@��\@�5?@��@���@��h@�/@��@�V@���@��@�9X@�(�@� �@��@�b@�  @�@�@~��@~�+@~E�@}��@}�@}�@}`B@}/@|�j@|I�@|�@{�m@{33@z�!@zn�@z=q@y�@y��@y7L@x��@x�`@x��@x�9@xr�@xA�@x1'@x  @w��@v�y@u@up�@u�@t�D@t(�@st�@s33@s@r�\@rn�@rn�@r�@q��@q%@pbN@pA�@p �@o�@o�;@o��@n�+@m/@l�D@lz�@lj@l9X@kdZ@j��@j=q@j�@j�@jJ@jJ@j-@j-@j=q@j=q@j=q@j=q@j�@j�@j�@j-@j-@j�@j�@jJ@jJ@i��@i�@i�7@h��@g�;@f�+@f@d��@c�
@ct�@c@b��@b~�@bM�@a��@a��@ahs@a&�@`��@`Ĝ@`��@`bN@`b@`b@_�@_�@_�P@^�+@]V@\��@\�@\z�@\I�@\9X@\1@[��@[��@[�F@[dZ@[S�@[S�@[S�@[@Z~�@ZM�@Yx�@Y%@X�9@X �@Wl�@W
=@V�y@V�y@W�@W;d@W�P@Wl�@WK�@W�@V�@V�R@V�+@U�@S�F@S33@SC�@S"�@RM�@Q�@Q�#@Q�@P�`@PbN@P �@Pr�@PĜ@Pb@O�@O�w@O;d@O
=@N�R@Nv�@L��@Lz�@L(�@K�m@K��@KS�@K"�@J�@I�^@I7L@H��@H  @G�w@G��@G|�@Gl�@G+@F��@FV@E�-@E?}@E�@E�@D�@C�F@C�@Ct�@B�@Ahs@@r�@@b@?��@?l�@?�@>V@=�T@=��@=��@=@=@=�-@=��@=p�@=O�@=O�@=V@<�/@<9X@;t�@:�@:�!@:M�@9��@9��@9��@9�@8�`@8�@7�@7��@7�P@7\)@7K�@7+@7�@6�@6v�@6E�@6@5�@5V@4�/@4��@4��@4Z@4(�@4�@3�
@3�
@3ƨ@3t�@2��@2n�@2M�@2-@2�@1�#@0��@0A�@0 �@/�@/�;@/��@/�w@/�@/�@/��@/�P@/\)@.��@.$�@-@-O�@,�@,�@,�D@,Z@,(�@+��@+�
@+�F@+S�@+"�@*�@*��@*��@)�7@(��@(bN@(  @'�@'\)@'+@'�@'
=@&�@&��@&E�@%��@%/@$�j@$��@$�D@$z�@$j@$9X@$(�@#�m@#ƨ@#��@"�H@"�\@"=q@!�@!�#@!��@!��@!x�@!&�@ �`@ ��@ �u@ �@ r�@ Q�@��@��@�w@�P@�@��@�y@ȴ@��@5?@$�@$�@{@@{@@�@�T@�h@�@�@p�@`B@�@��@I�@9X@ƨ@33@��@^5@M�@=q@-@�#@G�@��@�`@Ĝ@�u@r�@1'@�@�P@\)@+@
=@�y@�R@v�@E�@@��@�-@?}@��@z�@Z@(�@��@��@t�@C�@33@33@"�@o@�@��@�!@n�@=q@��@hs@7L@&�@�@%@��@�`@��@�9@r�@1'@�;@��@�P@;d@�@��@ȴ@��@��@v�@ff@E�@$�@@�T@�-@�@�@�@`B@?}@�/@�j@�j@�@�D@j@9X@�@1@��@�
@��@t�@t�@t�@dZ@33@33@o@
��@
-@
�@
J@	�@	�#@	�^@	��@	�7@	x�@	x�@	G�@	7L@	�@	%@	%@��@�`@Ĝ@��@�@Q�@Q�@A�@1'@ �@�;@�P@l�@�@�R@�+@v�@ff@ff@V@ff@ff@ff@ff@@�-@��@�h@p�@`B@p�@O�@�@�@�@��@�@�@1@�
@ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB{�By�Bw�Br�BhsB^5B\)Be`BgmBm�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bz�B�B�{B��B�uB��B��B�B��B��B��B��B��B��B��B��B��B��B�PB�B~�By�Be`BZBE�B)�BPB��B�
B��B�3B��B��B�Bw�BbNBR�BM�BH�BD�B?}B<jB1'B(�B"�BPB  B
�B
�ZB
�5B
��B
�dB
�B
��B
��B
�oB
�B
z�B
o�B
VB
J�B
>wB
8RB
.B
$�B
�B
VB
DB
B
B
  B	��B	�B	�ZB	�)B	��B	��B	ɺB	ĜB	�}B	�wB	�jB	�?B	�B	��B	��B	�1B	�%B	�B	y�B	r�B	jB	hsB	ffB	bNB	_;B	[#B	YB	XB	T�B	M�B	>wB	7LB	6FB	49B	33B	2-B	/B	,B	+B	%�B	 �B	�B	�B	�B	�B	hB	DB	1B	B��B�B�B�mB�HB�B�B��B��BƨBB�}B�XB�3B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�PB�DB�=B�%B�B�B~�B~�B}�B}�B|�B{�By�Bx�Bw�Bv�Bt�Bs�Bp�Bo�Bl�Bk�BiyBgmBcTBaHB`BB]/B[#BZBXBXBVBT�BS�BQ�BP�BM�BK�BI�BE�BD�BB�BA�B?}B=qB<jB<jB;dB;dB:^B8RB8RB49B33B1'B1'B0!B/B.B.B-B-B-B,B,B+B+B)�B(�B)�B(�B&�B,B)�B(�B(�B'�B)�B)�B)�B+B+B,B-B.B1'B0!B1'B0!B0!B2-B2-B33B33B33B33B33B49B5?B5?B5?B5?B5?B5?B7LB7LB7LB8RB7LB33B<jB;dB<jB<jB=qB>wB>wB>wB?}B@�B@�BC�BG�BI�BK�BO�BP�BQ�BT�BYBZB[#B]/B^5B^5B^5B_;BaHBcTBdZBdZBdZBdZBe`BgmBiyBjBk�Bl�Bn�Bn�Bp�Bq�Bq�Br�Bs�Bt�Bu�Bw�By�Bz�B}�B� B�B�%B�+B�=B�DB�DB�DB�JB�JB�JB�PB�PB�VB�hB��B��B��B��B��B��B��B��B��B�B�B�'B�9B�FB�^B�dB�wB��B��B��B��B��B��B��BBƨB��B��B��B��B��B�B�B�#B�/B�BB�NB�sB�B�B�B�B�B�B�B��B��B��B	  B	%B	1B	1B		7B	
=B	bB	bB	bB	hB	uB	uB	oB	{B	�B	�B	�B	�B	 �B	�B	 �B	#�B	$�B	'�B	,B	2-B	33B	49B	7LB	<jB	A�B	D�B	L�B	N�B	O�B	P�B	Q�B	R�B	T�B	XB	XB	ZB	ZB	\)B	]/B	_;B	dZB	gmB	gmB	hsB	hsB	hsB	iyB	iyB	iyB	jB	k�B	k�B	l�B	o�B	r�B	t�B	w�B	x�B	y�B	~�B	�B	�B	�B	�B	�1B	�PB	�PB	�VB	�VB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�FB	�FB	�XB	�^B	�^B	�^B	�XB	�XB	�RB	�XB	�XB	�dB	�qB	�wB	�wB	�}B	�}B	�}B	B	ÖB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B

=B

=B

=B

=B

=B
JB
DB
JB
DB
JB
VB
VB
VB
bB
bB
bB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
e`B
e`B
ffB
ffB
ffB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB{�By�Bw�Br�BhsB^5B\)Be`BgmBm�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bq�Bz�B�B�{B��B�uB��B��B�B��B��B��B��B��B��B��B��B��B��B�PB�B~�By�Be`BZBE�B)�BPB��B�
B��B�3B��B��B�Bw�BbNBR�BM�BH�BD�B?}B<jB1'B(�B"�BPB  B
�B
�ZB
�5B
��B
�dB
�B
��B
��B
�oB
�B
z�B
o�B
VB
J�B
>wB
8RB
.B
$�B
�B
VB
DB
B
B
  B	��B	�B	�ZB	�)B	��B	��B	ɺB	ĜB	�}B	�wB	�jB	�?B	�B	��B	��B	�1B	�%B	�B	y�B	r�B	jB	hsB	ffB	bNB	_;B	[#B	YB	XB	T�B	M�B	>wB	7LB	6FB	49B	33B	2-B	/B	,B	+B	%�B	 �B	�B	�B	�B	�B	hB	DB	1B	B��B�B�B�mB�HB�B�B��B��BƨBB�}B�XB�3B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�PB�DB�=B�%B�B�B~�B~�B}�B}�B|�B{�By�Bx�Bw�Bv�Bt�Bs�Bp�Bo�Bl�Bk�BiyBgmBcTBaHB`BB]/B[#BZBXBXBVBT�BS�BQ�BP�BM�BK�BI�BE�BD�BB�BA�B?}B=qB<jB<jB;dB;dB:^B8RB8RB49B33B1'B1'B0!B/B.B.B-B-B-B,B,B+B+B)�B(�B)�B(�B&�B,B)�B(�B(�B'�B)�B)�B)�B+B+B,B-B.B1'B0!B1'B0!B0!B2-B2-B33B33B33B33B33B49B5?B5?B5?B5?B5?B5?B7LB7LB7LB8RB7LB33B<jB;dB<jB<jB=qB>wB>wB>wB?}B@�B@�BC�BG�BI�BK�BO�BP�BQ�BT�BYBZB[#B]/B^5B^5B^5B_;BaHBcTBdZBdZBdZBdZBe`BgmBiyBjBk�Bl�Bn�Bn�Bp�Bq�Bq�Br�Bs�Bt�Bu�Bw�By�Bz�B}�B� B�B�%B�+B�=B�DB�DB�DB�JB�JB�JB�PB�PB�VB�hB��B��B��B��B��B��B��B��B��B�B�B�'B�9B�FB�^B�dB�wB��B��B��B��B��B��B��BBƨB��B��B��B��B��B�B�B�#B�/B�BB�NB�sB�B�B�B�B�B�B�B��B��B��B	  B	%B	1B	1B		7B	
=B	bB	bB	bB	hB	uB	uB	oB	{B	�B	�B	�B	�B	 �B	�B	 �B	#�B	$�B	'�B	,B	2-B	33B	49B	7LB	<jB	A�B	D�B	L�B	N�B	O�B	P�B	Q�B	R�B	T�B	XB	XB	ZB	ZB	\)B	]/B	_;B	dZB	gmB	gmB	hsB	hsB	hsB	iyB	iyB	iyB	jB	k�B	k�B	l�B	o�B	r�B	t�B	w�B	x�B	y�B	~�B	�B	�B	�B	�B	�1B	�PB	�PB	�VB	�VB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�FB	�FB	�XB	�^B	�^B	�^B	�XB	�XB	�RB	�XB	�XB	�dB	�qB	�wB	�wB	�}B	�}B	�}B	B	ÖB	ŢB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B

=B

=B

=B

=B

=B
JB
DB
JB
DB
JB
VB
VB
VB
bB
bB
bB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
e`B
e`B
ffB
ffB
ffB
gmB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220228120050                              AO  ARCAADJP                                                                    20220228120050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220228120050  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220228120050  QCF$                G�O�G�O�G�O�0               