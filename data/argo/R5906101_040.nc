CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-29T09:00:42Z creation      
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
resolution        =���   axis      Z        \  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  o4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ǈ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ڼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200629090042  20200629090042  5906101 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               (A   AO  7907                            2B  A   NAVIS_A                         1015                            170425                          863 @�$����1   @�$�33>�@+
��n��c�7KƧ�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      (A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B���B���B�  B�  B�33B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�
=@�
=A�A;�A[�A{�A�A�A�A�A�A���A�A�B�HB�HB�HB�HB&�HB.�HB6�HB>�HBF�HBN�HBV�HB^�HBf�HBn�HBv�HB~�HB�p�B�p�B�p�B�p�B�p�B�p�B���B�p�B���B�
>B�=qB�p�B�p�B���B�p�B���Bã�B�=qB�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�C�RC�RC�RC��C	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#��C%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm��Co�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D nD �DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D	nD	�D
nD
�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�DnD�D nD �D!nD!�D"nD"�D#nD#�D$nD$�D%nD%�D&nD&�D'nD'�D(nD(�D)nD)�D*nD*�D+nD+�D,nD,�D-nD-�D.nD.�D/nD/�D0nD0�D1nD1�D2nD2�D3nD3�D4nD4�D5nD5�D6nD6�D7nD7�D8nD8�D9nD9�D:nD:�D;nD;�D<nD<�D=nD=�D>nD>�D?nD?�D@nD@�DAnDA�DBnDB�DCnDC�DDnDD�DEnDE�DFtzDF�DGnDG�DHnDH�DInDI�DJnDJ�DKnDK�DLnDL�DMnDM�DNnDN�DOnDO�DPnDP�DQnDQ�DRnDR�DSnDS�DTnDT�DUnDU�DVnDV�DWnDW�DXnDX�DYnDY�DZnDZ�D[nD[�D\nD\�D]nD]�D^nD^�D_nD_�D`nD`�DanDa�DbnDb�DcnDc�DdnDd�DenDe�DfnDf�DgnDg�DhnDh�DinDi�DjnDj�DknDk�DlnDl�DmnDm�DnnDn�DonDo�DpnDp�DqnDq�DrnDr�DsnDs�DtnDt�DunDu�DvnDv�DwnDw�DxnDx�DynDy�DznDz�D{nD{�D|nD|�D}nD}�D~nD~�DnD�D�7
D�w
D��
D��
D�7
D�w
D��
D��
D�:=D�w
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
D���D��
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
D���D��
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
D�:=D�w
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
D��=D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�oA�bA�bA��A�oA��A�VA�bA�JA�VA�JA�{A��A�bA��A��A�bA�
=A�A�%A�1A�A���A���A��A��`A��;A��HA��TA��TA��A��
A�ƨAՉ7A�+Aԏ\A�hsA���A�E�A�XA�1A�I�A�7LA��A�  A�ĜA��A��/A��A��/A�5?A���A�v�A�^5A��
A��A��A��
A��
A��A��jA��RA�;dA��DA��A��RA�^5A��#AG�A|bNAx�DAuO�ArE�Ap1'Am�Aj�uAhĜAa;dA^�A\z�A[oAY�PAW�7AUx�AS�AP��AM7LAL9XAJ�DAGdZADȴAC��ADVABZA< �A9��A6��A1VA1A1�PA2r�A2�A3\)A3��A3ƨA21'A1��A2��A3VA2��A2��A2�A25?A1�#A0$�A*��A)33A(n�A(��A)�A)A*  A*�+A+%A*��A*�A(�jA&I�A$��A$1'A$r�A$A�A#+A"��A"�A!K�A ��A �A -A�-A"�A��A^5A  A��Al�A��AQ�A(�A(�A�A��A�7A�`An�AVA�HA��A�A�A�DAC�A�AƨA�A�9A1'AbA&�AoA�AĜA5?A��AA�A%A��A�A�RA?}A�A��A��A�A��A�`A+A
=A�!A�uAn�A{AƨAhsA�AȴA�DA5?A��AC�A��AjA  A��A�yA�AG�A
��A
^5A	�A	�7A	K�A	�A��Az�AZA5?A��A��Ap�A/A%Av�AA��AG�A"�A
=A�`A�9AVA1A  A��A�mA��A7LA�A��A1A�^Ax�A ��A ��A z�A A�A b@�K�@��!@�{@���@�O�@��@���@�Z@��@��y@�n�@�M�@�5?@���@��@�9X@�(�@�1'@��m@�;d@�n�@���@�O�@��@��`@��u@�@�z�@�  @��H@�M�@�@�V@��D@��m@�S�@�\@�J@�G�@���@�u@��@�dZ@�+@��@�`B@�%@�@� �@�|�@�
=@���@�v�@噚@�z�@�b@�|�@�^5@�7@�b@�"�@��@ާ�@�-@�&�@��@ۅ@�o@ڗ�@��@�@�X@�Ĝ@؛�@؋D@�r�@�A�@�K�@֟�@�E�@��@�@�p�@�O�@��@��@���@Ԭ@�Z@�b@���@�l�@�M�@Ѻ^@�p�@���@�r�@�1@���@�S�@��@Η�@�E�@�$�@��@���@͡�@��@���@��
@�"�@ʏ\@�ff@�=q@��T@�G�@�Z@ǶF@�C�@��@�M�@���@Ų-@�X@Ĵ9@�1@�l�@�o@���@\@�{@�`B@��9@�b@�C�@��H@�n�@�M�@�5?@���@��-@�V@���@��@�  @�"�@�~�@�$�@���@�X@�V@���@�z�@�z�@�(�@�33@���@�=q@��7@���@�Ĝ@��u@���@���@�ff@�M�@�$�@��-@�%@�Ĝ@��@�1@���@�t�@�+@�
=@���@�=q@��T@���@��@�p�@�O�@���@�\)@�@���@���@��\@�$�@���@�p�@�G�@�%@�Q�@�b@��m@��m@��m@���@��F@���@�C�@��R@�~�@�M�@��@���@��^@���@�O�@���@�I�@�9X@� �@�1@��@��w@���@�|�@�K�@��@���@�J@��^@��-@�p�@��@�Ĝ@�bN@�9X@��m@���@�K�@�"�@�~�@��@��-@���@��@�`B@�G�@��@��@�  @��@��
@�"�@���@�M�@��@�@�x�@�/@��D@��@�|�@�+@���@��@�ff@���@�O�@���@��D@�Z@�9X@�ƨ@�t�@�+@���@���@�~�@�^5@�E�@�$�@�{@�J@��@�X@��@�%@�Ĝ@���@�j@���@�S�@��H@�ff@�-@��@�hs@�/@�V@��`@��j@���@�Q�@��@�dZ@��H@�@��7@�G�@��`@���@��@��D@�  @��m@��
@��@�"�@���@�V@�$�@�{@�{@�{@�@���@�?}@�Ĝ@�1'@��m@��w@��@�+@��y@�ȴ@���@��R@�^5@�@���@���@��@�x�@�hs@�7L@�V@��D@�A�@�(�@�b@��@;d@~�+@~E�@}/@|��@|9X@{��@{��@{"�@z~�@y�#@yG�@xĜ@x��@x�u@x�@x�@xr�@xA�@w\)@v��@vȴ@vE�@u�-@u�h@u`B@t�@tj@t(�@t1@s�
@sC�@s"�@r�H@r��@r~�@r=q@q��@qx�@q7L@p��@pA�@o�P@o\)@o+@n�@n��@n$�@mp�@m?}@m/@lz�@l1@k�
@kS�@j~�@i��@i�7@i&�@h��@h�u@h��@h  @gK�@f��@f�+@fE�@fE�@f@e?}@d�@d9X@c�F@cC�@b=q@a�@a�^@a��@ax�@a7L@`��@_�@_|�@_l�@_;d@^�+@]@]?}@\�D@\9X@[��@[ƨ@[o@Z~�@ZM�@Y��@Y��@Y��@Y��@X��@XĜ@X�u@X �@W�P@Vȴ@VV@U�h@T��@S�m@SdZ@So@R�\@Q�@Qhs@Q&�@P��@P��@Pr�@Pb@Ol�@N�y@N$�@N@M�T@M��@M`B@L��@Lz�@L(�@L�@L1@Kƨ@KC�@J�H@J~�@I�@I�7@I7L@H��@G�P@G��@G�P@G;d@F��@F5?@E�-@E�h@Ep�@Ep�@E/@EV@D�@D�j@D��@DZ@C��@B�@BM�@A��@Ax�@A&�@@��@@Ĝ@@r�@@ �@?��@>��@>�+@>E�@=�T@=@=p�@=V@<��@<�D@<9X@;ƨ@;��@;��@;C�@:�@:�!@:^5@9�@9�#@9�^@9�7@9%@8��@8�`@8�`@8Ĝ@81'@7K�@6��@6�@6E�@65?@6{@5��@5�-@5�@5?}@4��@4�@49X@41@3t�@2�@1�7@1G�@1&�@1%@0��@0�`@0Ĝ@0r�@0Q�@0  @/�;@/�@/��@/�@.��@.�y@.�@.��@.v�@.ff@.$�@-�@,��@,9X@+�
@+��@+C�@*�@*~�@*n�@*-@)�@)��@)��@)x�@)G�@)%@(��@(b@'�@';d@&�R@&��@&�+@&E�@&5?@%�@%�@%`B@%?}@%/@%/@%�@$�@$��@$I�@$�@#�
@#�m@#��@#�@#dZ@#o@"�@"��@"�!@"~�@"n�@"=q@!�#@!��@!X@ �`@ �@ Q�@  �@�@|�@;d@��@�R@�+@$�@@�-@��@�h@��@p�@?}@V@�j@Z@��@�F@��@33@�@��@��@n�@=q@�@G�@��@Ĝ@�9@��@�u@�@bN@ �@  @  @�@�;@�;@�@l�@
=@�R@�+@v�@ff@V@5?@@@�@p�@O�@�@�/@�@�D@Z@9X@�@��@ƨ@�@dZ@C�@"�@�@��@�\@~�@^5@-@J@J@�@��@X@��@�`@�`@Ĝ@�9@��@��@��@�u@r�@ �@�;@��@�w@�w@�w@�P@l�@l�@\)@+@ȴ@V@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�oA�bA�bA��A�oA��A�VA�bA�JA�VA�JA�{A��A�bA��A��A�bA�
=A�A�%A�1A�A���A���A��A��`A��;A��HA��TA��TA��A��
A�ƨAՉ7A�+Aԏ\A�hsA���A�E�A�XA�1A�I�A�7LA��A�  A�ĜA��A��/A��A��/A�5?A���A�v�A�^5A��
A��A��A��
A��
A��A��jA��RA�;dA��DA��A��RA�^5A��#AG�A|bNAx�DAuO�ArE�Ap1'Am�Aj�uAhĜAa;dA^�A\z�A[oAY�PAW�7AUx�AS�AP��AM7LAL9XAJ�DAGdZADȴAC��ADVABZA< �A9��A6��A1VA1A1�PA2r�A2�A3\)A3��A3ƨA21'A1��A2��A3VA2��A2��A2�A25?A1�#A0$�A*��A)33A(n�A(��A)�A)A*  A*�+A+%A*��A*�A(�jA&I�A$��A$1'A$r�A$A�A#+A"��A"�A!K�A ��A �A -A�-A"�A��A^5A  A��Al�A��AQ�A(�A(�A�A��A�7A�`An�AVA�HA��A�A�A�DAC�A�AƨA�A�9A1'AbA&�AoA�AĜA5?A��AA�A%A��A�A�RA?}A�A��A��A�A��A�`A+A
=A�!A�uAn�A{AƨAhsA�AȴA�DA5?A��AC�A��AjA  A��A�yA�AG�A
��A
^5A	�A	�7A	K�A	�A��Az�AZA5?A��A��Ap�A/A%Av�AA��AG�A"�A
=A�`A�9AVA1A  A��A�mA��A7LA�A��A1A�^Ax�A ��A ��A z�A A�A b@�K�@��!@�{@���@�O�@��@���@�Z@��@��y@�n�@�M�@�5?@���@��@�9X@�(�@�1'@��m@�;d@�n�@���@�O�@��@��`@��u@�@�z�@�  @��H@�M�@�@�V@��D@��m@�S�@�\@�J@�G�@���@�u@��@�dZ@�+@��@�`B@�%@�@� �@�|�@�
=@���@�v�@噚@�z�@�b@�|�@�^5@�7@�b@�"�@��@ާ�@�-@�&�@��@ۅ@�o@ڗ�@��@�@�X@�Ĝ@؛�@؋D@�r�@�A�@�K�@֟�@�E�@��@�@�p�@�O�@��@��@���@Ԭ@�Z@�b@���@�l�@�M�@Ѻ^@�p�@���@�r�@�1@���@�S�@��@Η�@�E�@�$�@��@���@͡�@��@���@��
@�"�@ʏ\@�ff@�=q@��T@�G�@�Z@ǶF@�C�@��@�M�@���@Ų-@�X@Ĵ9@�1@�l�@�o@���@\@�{@�`B@��9@�b@�C�@��H@�n�@�M�@�5?@���@��-@�V@���@��@�  @�"�@�~�@�$�@���@�X@�V@���@�z�@�z�@�(�@�33@���@�=q@��7@���@�Ĝ@��u@���@���@�ff@�M�@�$�@��-@�%@�Ĝ@��@�1@���@�t�@�+@�
=@���@�=q@��T@���@��@�p�@�O�@���@�\)@�@���@���@��\@�$�@���@�p�@�G�@�%@�Q�@�b@��m@��m@��m@���@��F@���@�C�@��R@�~�@�M�@��@���@��^@���@�O�@���@�I�@�9X@� �@�1@��@��w@���@�|�@�K�@��@���@�J@��^@��-@�p�@��@�Ĝ@�bN@�9X@��m@���@�K�@�"�@�~�@��@��-@���@��@�`B@�G�@��@��@�  @��@��
@�"�@���@�M�@��@�@�x�@�/@��D@��@�|�@�+@���@��@�ff@���@�O�@���@��D@�Z@�9X@�ƨ@�t�@�+@���@���@�~�@�^5@�E�@�$�@�{@�J@��@�X@��@�%@�Ĝ@���@�j@���@�S�@��H@�ff@�-@��@�hs@�/@�V@��`@��j@���@�Q�@��@�dZ@��H@�@��7@�G�@��`@���@��@��D@�  @��m@��
@��@�"�@���@�V@�$�@�{@�{@�{@�@���@�?}@�Ĝ@�1'@��m@��w@��@�+@��y@�ȴ@���@��R@�^5@�@���@���@��@�x�@�hs@�7L@�V@��D@�A�@�(�@�b@��@;d@~�+@~E�@}/@|��@|9X@{��@{��@{"�@z~�@y�#@yG�@xĜ@x��@x�u@x�@x�@xr�@xA�@w\)@v��@vȴ@vE�@u�-@u�h@u`B@t�@tj@t(�@t1@s�
@sC�@s"�@r�H@r��@r~�@r=q@q��@qx�@q7L@p��@pA�@o�P@o\)@o+@n�@n��@n$�@mp�@m?}@m/@lz�@l1@k�
@kS�@j~�@i��@i�7@i&�@h��@h�u@h��@h  @gK�@f��@f�+@fE�@fE�@f@e?}@d�@d9X@c�F@cC�@b=q@a�@a�^@a��@ax�@a7L@`��@_�@_|�@_l�@_;d@^�+@]@]?}@\�D@\9X@[��@[ƨ@[o@Z~�@ZM�@Y��@Y��@Y��@Y��@X��@XĜ@X�u@X �@W�P@Vȴ@VV@U�h@T��@S�m@SdZ@So@R�\@Q�@Qhs@Q&�@P��@P��@Pr�@Pb@Ol�@N�y@N$�@N@M�T@M��@M`B@L��@Lz�@L(�@L�@L1@Kƨ@KC�@J�H@J~�@I�@I�7@I7L@H��@G�P@G��@G�P@G;d@F��@F5?@E�-@E�h@Ep�@Ep�@E/@EV@D�@D�j@D��@DZ@C��@B�@BM�@A��@Ax�@A&�@@��@@Ĝ@@r�@@ �@?��@>��@>�+@>E�@=�T@=@=p�@=V@<��@<�D@<9X@;ƨ@;��@;��@;C�@:�@:�!@:^5@9�@9�#@9�^@9�7@9%@8��@8�`@8�`@8Ĝ@81'@7K�@6��@6�@6E�@65?@6{@5��@5�-@5�@5?}@4��@4�@49X@41@3t�@2�@1�7@1G�@1&�@1%@0��@0�`@0Ĝ@0r�@0Q�@0  @/�;@/�@/��@/�@.��@.�y@.�@.��@.v�@.ff@.$�@-�@,��@,9X@+�
@+��@+C�@*�@*~�@*n�@*-@)�@)��@)��@)x�@)G�@)%@(��@(b@'�@';d@&�R@&��@&�+@&E�@&5?@%�@%�@%`B@%?}@%/@%/@%�@$�@$��@$I�@$�@#�
@#�m@#��@#�@#dZ@#o@"�@"��@"�!@"~�@"n�@"=q@!�#@!��@!X@ �`@ �@ Q�@  �@�@|�@;d@��@�R@�+@$�@@�-@��@�h@��@p�@?}@V@�j@Z@��@�F@��@33@�@��@��@n�@=q@�@G�@��@Ĝ@�9@��@�u@�@bN@ �@  @  @�@�;@�;@�@l�@
=@�R@�+@v�@ff@V@5?@@@�@p�@O�@�@�/@�@�D@Z@9X@�@��@ƨ@�@dZ@C�@"�@�@��@�\@~�@^5@-@J@J@�@��@X@��@�`@�`@Ĝ@�9@��@��@��@�u@r�@ �@�;@��@�w@�w@�w@�P@l�@l�@\)@+@ȴ@V@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BB�;B�BB�;B�;B�BB�;B�BB�;B�BB�;B�BB�;B�;B�BB�BB�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�NB�TB�TB�HB�NB�TB�NB�NB�;B�#B�
B��BĜB�B��B�B�fB��B	uB	"�B	(�B	]/B	�B
P�B
�LB
�BB
�mB
�mB
�`B
�HB
��B
�jB
�B
��B
�\B
�B
r�B
\)B
C�B
-B
�B
JB	��B	�B	�}B	��B	��B	�JB	�DB	�%B	�B	�B	�oB	��B	��B	��B	��B	�B	��B	�'B	��B	��B	��B	�oB	�7B	�DB	��B	�-B	�dB	��B	�1B	o�B	G�B	ZB	y�B	��B	�?B	ƨB	��B	�B	�B	�B
PB
hB
hB
bB
PB
	7B
B	��B	��B	�jB	�dB	��B	�5B	�B
VB
!�B
7LB
9XB
>wB
=qB
+B
�B
 �B
0!B
;dB
<jB
A�B
=qB
:^B
:^B
A�B
A�B
?}B
>wB
?}B
@�B
@�B
C�B
D�B
D�B
D�B
H�B
Q�B
Q�B
Q�B
P�B
M�B
N�B
R�B
cTB
x�B
}�B
}�B
�1B
��B
�{B
�JB
�B
� B
|�B
�B
y�B
x�B
�B
�B
� B
{�B
�%B
�B
�B
v�B
n�B
n�B
x�B
x�B
w�B
p�B
k�B
jB
o�B
w�B
y�B
y�B
x�B
x�B
v�B
w�B
v�B
v�B
t�B
r�B
r�B
p�B
m�B
k�B
jB
hsB
dZB
aHB
YB
W
B
W
B
T�B
R�B
P�B
O�B
O�B
M�B
M�B
N�B
M�B
K�B
J�B
I�B
H�B
G�B
F�B
D�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
@�B
A�B
>wB
=qB
<jB
<jB
:^B
:^B
9XB
9XB
8RB
7LB
7LB
6FB
7LB
6FB
6FB
5?B
5?B
49B
33B
33B
33B
33B
49B
33B
33B
33B
33B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
0!B
/B
+B
)�B
(�B
&�B
'�B
+B
,B
)�B
&�B
%�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
)�B
)�B
)�B
(�B
(�B
)�B
(�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
-B
.B
.B
.B
-B
-B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
6FB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
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
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
VB
T�B
T�B
VB
VB
VB
VB
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
\)B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
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
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BB�;B�BB�;B�;B�BB�;B�BB�;B�BB�;B�BB�;B�;B�BB�BB�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�NB�TB�TB�HB�NB�TB�NB�NB�;B�#B�
B��BĜB�B��B�B�fB��B	uB	"�B	(�B	]/B	�B
P�B
�LB
�BB
�mB
�mB
�`B
�HB
��B
�jB
�B
��B
�\B
�B
r�B
\)B
C�B
-B
�B
JB	��B	�B	�}B	��B	��B	�JB	�DB	�%B	�B	�B	�oB	��B	��B	��B	��B	�B	��B	�'B	��B	��B	��B	�oB	�7B	�DB	��B	�-B	�dB	��B	�1B	o�B	G�B	ZB	y�B	��B	�?B	ƨB	��B	�B	�B	�B
PB
hB
hB
bB
PB
	7B
B	��B	��B	�jB	�dB	��B	�5B	�B
VB
!�B
7LB
9XB
>wB
=qB
+B
�B
 �B
0!B
;dB
<jB
A�B
=qB
:^B
:^B
A�B
A�B
?}B
>wB
?}B
@�B
@�B
C�B
D�B
D�B
D�B
H�B
Q�B
Q�B
Q�B
P�B
M�B
N�B
R�B
cTB
x�B
}�B
}�B
�1B
��B
�{B
�JB
�B
� B
|�B
�B
y�B
x�B
�B
�B
� B
{�B
�%B
�B
�B
v�B
n�B
n�B
x�B
x�B
w�B
p�B
k�B
jB
o�B
w�B
y�B
y�B
x�B
x�B
v�B
w�B
v�B
v�B
t�B
r�B
r�B
p�B
m�B
k�B
jB
hsB
dZB
aHB
YB
W
B
W
B
T�B
R�B
P�B
O�B
O�B
M�B
M�B
N�B
M�B
K�B
J�B
I�B
H�B
G�B
F�B
D�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
@�B
A�B
>wB
=qB
<jB
<jB
:^B
:^B
9XB
9XB
8RB
7LB
7LB
6FB
7LB
6FB
6FB
5?B
5?B
49B
33B
33B
33B
33B
49B
33B
33B
33B
33B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
0!B
/B
+B
)�B
(�B
&�B
'�B
+B
,B
)�B
&�B
%�B
#�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
$�B
#�B
#�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
)�B
)�B
)�B
(�B
(�B
)�B
(�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
-B
.B
.B
.B
-B
-B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
6FB
7LB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
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
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
VB
T�B
T�B
VB
VB
VB
VB
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
\)B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
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
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�+B
�+B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200629090042                              AO  ARCAADJP                                                                    20200629090042    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200629090042  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200629090042  QCF$                G�O�G�O�G�O�0               