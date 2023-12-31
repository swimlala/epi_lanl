CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-16T00:35:17Z creation;2017-05-16T00:35:20Z conversion to V3.1;2019-12-19T08:07:33Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20170516003517  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  I2_0577_119                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��, 1   @���l @2�/�{J#�dh����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@xQ�@�@�\AG�A9G�AYG�AyG�A���A���A���A���Ạ�Aܣ�A��A���BQ�BQ�BQ�BQ�B&Q�B.Q�B6Q�B>Q�BFQ�BNQ�BVQ�B^Q�BfQ�BnQ�BvQ�B~Q�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��qC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=CǽqC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C׽qC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D eD �DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D	eD	�D
eD
�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D eD �D!eD!�D"eD"�D#eD#�D$eD$�D%eD%�D&eD&�D'eD'�D(eD(�D)eD)�D*eD*�D+eD+�D,eD,�D-eD-�D.eD.�D/eD/�D0eD0�D1eD1�D2eD2�D3eD3�D4eD4�D5eD5�D6eD6�D7eD7�D8eD8�D9eD9�D:eD:�D;eD;�D<eD<�D=eD=�D>eD>�D?eD?�D@eD@�DAeDA�DBeDB�DCeDC�DDeDD�DEeDE�DFeDF�DGeDG�DHeDH�DIeDI�DJeDJ�DKeDK�DLeDL�DMeDM�DNeDN�DOeDO�DPeDP�DQeDQ�DReDR�DSeDS�DTeDT�DUeDU�DVeDV�DWeDW�DXeDX�DYeDY�DZeDZ�D[eD[�D\eD\�D]eD]�D^eD^�D_eD_�D`eD`�DaeDa�DbeDb�DceDc�DdeDd�DeeDe�DfeDf�DgeDg�DheDh�DieDi�DjeDj�DkeDk�DleDl�DmeDm�DneDn�DoeDo�DpeDp�DqeDq�DreDr�DseDs�DteDt�DueDu�DveDv�DweDw�DxeDx�DyeDy�Dz^�Dz�D{eD{�D|eD|�D}eD}�D~eD~�DeD�D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D²�D��D�2�D�r�Dò�D��D�2�D�r�DĲ�D��D�2�D�r�DŲ�D��D�2�D�r�DƲ�D��D�2�D�r�Dǲ�D��D�2�D�r�DȲ�D��D�2�D�r�Dɲ�D��D�2�D�r�Dʲ�D��D�2�D�r�D˲�D��D�2�D�r�D̲�D��D�2�D�r�DͲ�D��D�2�D�r�Dβ�D��D�2�D�r�Dϲ�D��D�2�D�r�Dв�D��D�2�D�r�DѲ�D��D�2�D�r�DҲ�D��D�2�D�r�DӲ�D��D�2�D�r�DԲ�D��D�2�D�r�Dղ�D��D�2�D�r�Dֲ�D��D�2�D�r�Dײ�D��D�2�D�r�Dز�D��D�2�D�r�Dٲ�D��D�2�D�r�Dڲ�D��D�2�D�r�D۲�D��D�2�D�r�Dܲ�D��D�2�D�r�Dݲ�D��D�2�D�r�D޲�D��D�2�D�r�D߲�D��D�2�D�r�DಏD��D�2�D�r�D᲏D��D�2�D�r�DⲏD��D�2�D�r�D㲏D��D�2�D�r�D䲏D��D�2�D�r�D岏D��D�2�D�r�D沏D��D�2�D�r�D粏D��D�2�D�r�D貏D��D�2�D�r�D鲏D��D�2�D�r�D겏D��D�2�D�r�D벏D��D�2�D�r�D첏D��D�2�D�r�D���D��D�2�D�r�DD��D�2�D�r�DﲏD��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D���D�)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aԉ7Aԉ7Aԉ7AԅA�~�A�z�A�v�A�n�A�jA�l�A�l�A�n�A�n�A�n�A�l�A�l�A�jA�hsA�jA�l�A�ffA�;dA�  A��/A�O�A�AѬA�=qA�-A�  Aϗ�A�M�A�~�A��ȂhA���A��A���A��/Aʴ9AʅA�7LA�VA���A���Aɧ�A�G�A�$�A�oA�%A��HAȶFAț�A�^5A�I�A�+A���A�n�A���A�JA�n�A��
A�Q�A�VAøRA�1'A���A�r�A��TA��9A���A��A��/A�VA��\A�l�A��wA���A��DA�9XA�1'A��A�9XA���A��FA�  A�jA��A�dZA��A���A���A�\)A���A��A��hA�~�A��A���A�v�A�1A��DA�t�A��A���A���A�I�A�/A�1A��#A��A���A�+A�/A���A�`BA���A�ƨA���A��A�M�A���A���A��A�z�A~VAz�jAx�Au�-Asx�AqApȴAop�Ak�^AhVAf(�Ae&�AdAb-Aa�FAa�7A_�FA\�jA[��AZz�AYVAW�;AVv�AU�mAU�ATĜAQXAL�AH�AFr�AES�AD�AB��AAt�A?7LA=`BA<�/A:�9A8�A7�
A6��A4ZA2v�A0��A/t�A.VA-|�A+��A*�jA*�\A)�TA(��A(�\A'��A'A&�DA&1A%K�A$JA#t�A"�A!VA 1'A��A
=AA�A�^A%AE�A��AȴA��A%AffA�#A
=A��AJAXAv�A�At�A�+AAl�A5?A�A�uAE�A�FA"�A
��A
ZA
1A	l�A�RAz�AI�AbAx�AĜA��A�uA��A�A$�A�A��A�7A �`@��@�/@���@���@���@�&�@���@���@�ƨ@��7@�@�@�@�I�@�;d@��@�l�@柾@噚@�
=@���@��H@�1'@�r�@�l�@�^5@��@��@�Q�@�t�@���@އ+@�%@��@�K�@�@�X@�?}@ׅ@��#@�G�@� �@җ�@��@��
@Ώ\@͡�@́@�?}@̛�@�9X@���@ȋD@Ǖ�@�@�v�@��@�hs@�A�@�K�@��@�+@�
=@�  @�/@��T@���@��T@�x�@�33@�1@��P@�@�=q@�x�@���@��@�\)@�"�@��H@��T@��j@�bN@�  @�t�@�"�@��y@�`B@�bN@��@�1@���@�ƨ@���@�{@�V@���@��@�|�@��@��\@�@��T@���@���@�X@�V@�%@��`@�Ĝ@�z�@���@��!@�$�@���@�x�@��^@�5?@�V@�J@��h@�G�@�V@��m@�|�@��@��@��H@��+@�5?@��-@�X@�7L@��@�Q�@��@���@���@��@��@���@��!@��+@�=q@���@�x�@�G�@���@�A�@��w@�dZ@�S�@�t�@�K�@�o@��!@�-@�=q@�J@��h@��7@��@��j@��j@��D@�j@��;@�S�@�o@���@�~�@�$�@���@��@�@��7@�X@�O�@�7L@��@�V@��@��9@�r�@�1'@���@��P@�dZ@�l�@�\)@��@�ff@�5?@��@�@��#@���@�x�@�hs@�X@�?}@�&�@��@�V@���@��`@��@���@��@��D@�bN@�I�@�1@���@���@�|�@�S�@�S�@�K�@�
=@��y@���@���@�G�@�7L@�O�@��@�b@���@��w@��F@��@��@�+@�C�@�dZ@�t�@�C�@��@��y@�v�@�J@���@���@��@�`B@�%@���@�Ĝ@��j@���@�bN@�A�@�  @��F@��@�33@��H@�ȴ@�v�@�@�x�@�O�@�%@�z�@�9X@�ƨ@��y@���@�ff@�J@��@��-@�x�@�`B@�7L@�%@���@�z�@�r�@�j@�Z@�(�@��@��;@��F@�|�@�"�@��y@�ȴ@�n�@�=q@�-@�$�@�@��T@���@�V@��u@�j@�Z@�A�@��@
=@~V@~@}�-@}�@|�@|�@|�@|Z@{"�@z�!@z��@zM�@z-@z�@y��@y%@x �@w�@w��@v��@v�+@vff@v5?@u��@u��@u�-@u?}@t�D@t�@t1@s�m@s��@sdZ@sS�@sC�@r��@rM�@r-@r-@q��@q&�@p�9@pQ�@pb@pb@o�@o�P@oK�@n��@nV@m��@m`B@m�@l�@l�D@l(�@k33@j�!@j~�@j�@i�#@i7L@h�@h1'@h  @g�@g�@g�@fv�@f@e`B@e/@e/@e�@d�/@d�j@d�D@c��@ct�@c"�@b��@bM�@a�@a��@ax�@a&�@`�9@` �@_�;@_�P@_�@^�y@^�R@^ff@^{@]��@]��@]O�@]V@\�D@\Z@\�@[�m@[�
@[ƨ@[C�@Z�!@Y��@Y�7@Y�@Y%@Wl�@Vȴ@V�R@V�+@V5?@U�@U@Up�@U/@U/@U/@T�@S"�@R��@RM�@R-@Q�#@Q�7@Q&�@P�@Pb@O�;@O��@O;d@M��@MV@L��@K�m@K�@J�H@J�\@J^5@J-@I�^@Ihs@H��@H��@HbN@H  @H  @G�@GK�@Fȴ@FV@E@E�@EO�@EV@D��@D�@D�D@Dz�@DI�@C�m@CS�@C"�@B�@B��@B��@B�!@B�@A��@A�7@A7L@@��@@bN@@A�@@1'@@ �@@  @?�;@?��@?�@?�P@?l�@?\)@?K�@?;d@?;d@?�@>ȴ@>ff@>5?@>{@>{@>@=@=�@=O�@=/@<��@<��@<�D@<z�@<j@<Z@<I�@<I�@<�@<�@;�
@;��@;S�@;o@:��@:�!@:n�@:-@9�#@9�7@97L@8��@8�u@7�;@7�@7;d@7+@6��@6V@5@4��@4��@4z�@4j@4Z@4I�@3ƨ@3t�@2�H@2�\@2-@1�@1x�@0�@/��@/K�@.�+@-�@,�@,��@,(�@+ƨ@+33@+@*�!@*�@)�@)��@)�^@)��@)&�@(�9@(Q�@(Q�@(Q�@(Q�@(1'@'��@'
=@&�+@&5?@%�T@%��@%V@%V@$��@$�@$(�@#�m@#�
@#�F@#��@#dZ@"�@"�!@"~�@"M�@"=q@"-@"�@"�@!��@!7L@ �`@ �9@ ��@ �u@ �@ �@ r�@ r�@ Q�@�w@;d@+@�@��@��@�+@V@��@O�@/@�@�@�@�@�@�@V@�@�@Z@9X@�m@ƨ@��@�@�@�@�@�@t�@dZ@S�@"�@��@��@�!@�!@=q@��@�@��@��@�7@�`@��@��@��@�u@�@bN@1'@�;@��@|�@l�@\)@K�@K�@;d@+@��@ȴ@��@v�@V@@�h@`B@O�@/@��@��@�@��@��@�D@z�@j@I�@�@��@dZ@C�@33@o@�H@�\@~�@n�@n�@M�@-@��@��@�7@x�@X@&�@��@Ĝ@��@�@Q�@1'@ �@b@�;@�w@��@l�@K�@��@��@ff@$�@�@��@@�h@/@�@��@��@��@j@9X@1@�F@�@t�@dZ@S�@33@o@
�@
��@
��@
~�@
^5@
-@	��@	�@	�#@	�#@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aԉ7Aԉ7Aԉ7AԅA�~�A�z�A�v�A�n�A�jA�l�A�l�A�n�A�n�A�n�A�l�A�l�A�jA�hsA�jA�l�A�ffA�;dA�  A��/A�O�A�AѬA�=qA�-A�  Aϗ�A�M�A�~�A��ȂhA���A��A���A��/Aʴ9AʅA�7LA�VA���A���Aɧ�A�G�A�$�A�oA�%A��HAȶFAț�A�^5A�I�A�+A���A�n�A���A�JA�n�A��
A�Q�A�VAøRA�1'A���A�r�A��TA��9A���A��A��/A�VA��\A�l�A��wA���A��DA�9XA�1'A��A�9XA���A��FA�  A�jA��A�dZA��A���A���A�\)A���A��A��hA�~�A��A���A�v�A�1A��DA�t�A��A���A���A�I�A�/A�1A��#A��A���A�+A�/A���A�`BA���A�ƨA���A��A�M�A���A���A��A�z�A~VAz�jAx�Au�-Asx�AqApȴAop�Ak�^AhVAf(�Ae&�AdAb-Aa�FAa�7A_�FA\�jA[��AZz�AYVAW�;AVv�AU�mAU�ATĜAQXAL�AH�AFr�AES�AD�AB��AAt�A?7LA=`BA<�/A:�9A8�A7�
A6��A4ZA2v�A0��A/t�A.VA-|�A+��A*�jA*�\A)�TA(��A(�\A'��A'A&�DA&1A%K�A$JA#t�A"�A!VA 1'A��A
=AA�A�^A%AE�A��AȴA��A%AffA�#A
=A��AJAXAv�A�At�A�+AAl�A5?A�A�uAE�A�FA"�A
��A
ZA
1A	l�A�RAz�AI�AbAx�AĜA��A�uA��A�A$�A�A��A�7A �`@��@�/@���@���@���@�&�@���@���@�ƨ@��7@�@�@�@�I�@�;d@��@�l�@柾@噚@�
=@���@��H@�1'@�r�@�l�@�^5@��@��@�Q�@�t�@���@އ+@�%@��@�K�@�@�X@�?}@ׅ@��#@�G�@� �@җ�@��@��
@Ώ\@͡�@́@�?}@̛�@�9X@���@ȋD@Ǖ�@�@�v�@��@�hs@�A�@�K�@��@�+@�
=@�  @�/@��T@���@��T@�x�@�33@�1@��P@�@�=q@�x�@���@��@�\)@�"�@��H@��T@��j@�bN@�  @�t�@�"�@��y@�`B@�bN@��@�1@���@�ƨ@���@�{@�V@���@��@�|�@��@��\@�@��T@���@���@�X@�V@�%@��`@�Ĝ@�z�@���@��!@�$�@���@�x�@��^@�5?@�V@�J@��h@�G�@�V@��m@�|�@��@��@��H@��+@�5?@��-@�X@�7L@��@�Q�@��@���@���@��@��@���@��!@��+@�=q@���@�x�@�G�@���@�A�@��w@�dZ@�S�@�t�@�K�@�o@��!@�-@�=q@�J@��h@��7@��@��j@��j@��D@�j@��;@�S�@�o@���@�~�@�$�@���@��@�@��7@�X@�O�@�7L@��@�V@��@��9@�r�@�1'@���@��P@�dZ@�l�@�\)@��@�ff@�5?@��@�@��#@���@�x�@�hs@�X@�?}@�&�@��@�V@���@��`@��@���@��@��D@�bN@�I�@�1@���@���@�|�@�S�@�S�@�K�@�
=@��y@���@���@�G�@�7L@�O�@��@�b@���@��w@��F@��@��@�+@�C�@�dZ@�t�@�C�@��@��y@�v�@�J@���@���@��@�`B@�%@���@�Ĝ@��j@���@�bN@�A�@�  @��F@��@�33@��H@�ȴ@�v�@�@�x�@�O�@�%@�z�@�9X@�ƨ@��y@���@�ff@�J@��@��-@�x�@�`B@�7L@�%@���@�z�@�r�@�j@�Z@�(�@��@��;@��F@�|�@�"�@��y@�ȴ@�n�@�=q@�-@�$�@�@��T@���@�V@��u@�j@�Z@�A�@��@
=@~V@~@}�-@}�@|�@|�@|�@|Z@{"�@z�!@z��@zM�@z-@z�@y��@y%@x �@w�@w��@v��@v�+@vff@v5?@u��@u��@u�-@u?}@t�D@t�@t1@s�m@s��@sdZ@sS�@sC�@r��@rM�@r-@r-@q��@q&�@p�9@pQ�@pb@pb@o�@o�P@oK�@n��@nV@m��@m`B@m�@l�@l�D@l(�@k33@j�!@j~�@j�@i�#@i7L@h�@h1'@h  @g�@g�@g�@fv�@f@e`B@e/@e/@e�@d�/@d�j@d�D@c��@ct�@c"�@b��@bM�@a�@a��@ax�@a&�@`�9@` �@_�;@_�P@_�@^�y@^�R@^ff@^{@]��@]��@]O�@]V@\�D@\Z@\�@[�m@[�
@[ƨ@[C�@Z�!@Y��@Y�7@Y�@Y%@Wl�@Vȴ@V�R@V�+@V5?@U�@U@Up�@U/@U/@U/@T�@S"�@R��@RM�@R-@Q�#@Q�7@Q&�@P�@Pb@O�;@O��@O;d@M��@MV@L��@K�m@K�@J�H@J�\@J^5@J-@I�^@Ihs@H��@H��@HbN@H  @H  @G�@GK�@Fȴ@FV@E@E�@EO�@EV@D��@D�@D�D@Dz�@DI�@C�m@CS�@C"�@B�@B��@B��@B�!@B�@A��@A�7@A7L@@��@@bN@@A�@@1'@@ �@@  @?�;@?��@?�@?�P@?l�@?\)@?K�@?;d@?;d@?�@>ȴ@>ff@>5?@>{@>{@>@=@=�@=O�@=/@<��@<��@<�D@<z�@<j@<Z@<I�@<I�@<�@<�@;�
@;��@;S�@;o@:��@:�!@:n�@:-@9�#@9�7@97L@8��@8�u@7�;@7�@7;d@7+@6��@6V@5@4��@4��@4z�@4j@4Z@4I�@3ƨ@3t�@2�H@2�\@2-@1�@1x�@0�@/��@/K�@.�+@-�@,�@,��@,(�@+ƨ@+33@+@*�!@*�@)�@)��@)�^@)��@)&�@(�9@(Q�@(Q�@(Q�@(Q�@(1'@'��@'
=@&�+@&5?@%�T@%��@%V@%V@$��@$�@$(�@#�m@#�
@#�F@#��@#dZ@"�@"�!@"~�@"M�@"=q@"-@"�@"�@!��@!7L@ �`@ �9@ ��@ �u@ �@ �@ r�@ r�@ Q�@�w@;d@+@�@��@��@�+@V@��@O�@/@�@�@�@�@�@�@V@�@�@Z@9X@�m@ƨ@��@�@�@�@�@�@t�@dZ@S�@"�@��@��@�!@�!@=q@��@�@��@��@�7@�`@��@��@��@�u@�@bN@1'@�;@��@|�@l�@\)@K�@K�@;d@+@��@ȴ@��@v�@V@@�h@`B@O�@/@��@��@�@��@��@�D@z�@j@I�@�@��@dZ@C�@33@o@�H@�\@~�@n�@n�@M�@-@��@��@�7@x�@X@&�@��@Ĝ@��@�@Q�@1'@ �@b@�;@�w@��@l�@K�@��@��@ff@$�@�@��@@�h@/@�@��@��@��@j@9X@1@�F@�@t�@dZ@S�@33@o@
�@
��@
��@
~�@
^5@
-@	��@	�@	�#@	�#@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�DB
�B
�B
�B
� B
�B
�B
x�B
|�B
�B
�B
�B
�+B
�\B
�hB
�uB
��B
��B
��B
��B
��B
��B
�B
�9B
�RB
�^B
�}B
ǮB
�`B
��B{B!�B8RBl�B�{B�B��B��B��B��B�
B�;B�`B�mB�sB�BB�B�B�B"�B%�B/B/B0!B49B49B33B2-B49B7LB8RB6FB6FB5?B5?B49B33B33B2-B0!B1'B,B&�B$�B#�B�B�B{B
=B��B��B�B�HB�BɺB�qB��B�DB�Bq�B]/B@�B,B�BB
�sB
�)B
�
B
�9B
o�B
>wB
oB	��B	�/B	ƨB	�^B	�B	��B	��B	�+B	q�B	ffB	`BB	\)B	Q�B	L�B	I�B	E�B	5?B	/B	)�B	%�B	�B	�B	�B	+B	�B	JB��B�B�NB�sB�B�B�B�yB�;B�)B�B��B��BŢB�^B�LB�XB�LB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�B�B�-B�B��B��B�B�B�B�B�B�B��B��B�B�3B�3B�9B�9B�9B�-B�-B�RBBBÖBĜBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B�B��B��B��B�
B��B��BȴBÖB�^B�XB�XB�XB�FB�FB�^B�jBŢBǮBȴBȴBȴBȴBB�^B�RB�9B��B��B��B��B��B��B��B�^BBǮB��B��B��B��B�B�B�B�B�B�#B�5B�TB�`B�B�B�B�B��B��B��B��B��B��B	B	%B	
=B	%B	B	+B	1B	1B	1B	+B	+B	
=B	JB	uB	�B	�B	+B	33B	49B	5?B	33B	1'B	+B	+B	)�B	'�B	$�B	$�B	#�B	"�B	"�B	"�B	!�B	%�B	+B	.B	1'B	2-B	2-B	5?B	8RB	8RB	8RB	7LB	7LB	;dB	=qB	A�B	C�B	E�B	F�B	I�B	L�B	R�B	R�B	R�B	S�B	VB	VB	VB	VB	VB	W
B	VB	T�B	T�B	W
B	W
B	_;B	e`B	iyB	jB	l�B	l�B	o�B	r�B	t�B	w�B	w�B	y�B	z�B	{�B	{�B	�B	�B	�%B	�+B	�%B	�1B	�=B	�JB	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�FB	�LB	�RB	�RB	�RB	�XB	�XB	�dB	�jB	�qB	�wB	��B	B	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B	��B	��B	��B
  B	��B	��B	��B	��B
  B
  B
B
B
B
%B
1B

=B
DB
DB
DB

=B

=B

=B
DB
DB
PB
VB
VB
VB
VB
\B
bB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
uB
�B
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
!�B
"�B
"�B
!�B
!�B
"�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
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
,B
-B
.B
.B
-B
.B
.B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
K�B
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
L�B
L�B
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
N�B
N�B
N�B
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
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
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
XB
YB
YB
ZB
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
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
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
ffB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
n�B
n�B
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
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
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
s�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
x�B
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
{�B
{�B
{�B
{�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�\B
��B
��B
��B
��B
�B
��B
��B
�AB
��B
{B
~�B
�AB
�B
�uB
��B
��B
��B
�B
�+B
�dB
�4B
�@B
�`B
��B
��B
��B
��B
��B
� B
�fB
�B
�RB�B"hB9�Bm�B��B�'B�B�SB��B��BؓB�B�B�XB�B��B�B~BIB!|B%FB)�B0�B1'B2�B5B4�B4TB4�B8B:�B9�B7�B7�B6zB6+B4�B4B49B3�B3B5�B.IB(>B&�B&�B#BWB�B6B�PB�B�nB�&B�B�6B�B�)B�PB��Bt�B`BBB�B.cB�BzB
�B
��B
��B
�PB
u�B
C�B
�B	�B	�\B	ɆB	��B	��B	��B	�'B	�)B	t9B	h
B	bB	^5B	R�B	NB	LdB	H�B	6�B	0�B	,B	'�B	!bB	�B	�B	-]B	$�B	 B�$B�}B�@B�eB�B��B�cB�B��B�B�xB��B�BȴB�B�rB�B�	B��B�hB�iB��B�0B�KB��B�$B�$B��B�B�LB��B�=B��B�9B�oB��B�
B�=B�)B�IB�IB�IB�qB��B��B�)B�9B�nB�%B�tB��B��B�B��B��BðB��B�tB�7BɠBɆB��BˬB˒B̘B˒B��B��BӏBԕBյB�$B�B҉BңBؓB�@B�BBʌB�B�B��B��B��B��B��B��B�BB�tB�BɆB�=B��B�rBðB�JB��B�fB��B��B��B�HB�B��B��B��B�{BȚB�^B̳B��B��B֡B��B�?B��B�	B�)B��B��B��B��B��B��B�B�B��B��B��B�JB�qB	�B	B	�B	+B	B	�B	�B	�B	�B	1B	�B	
�B	~B	�B	2B	OB	*�B	3hB	4�B	6+B	5%B	3B	+�B	+�B	*�B	(�B	%�B	%�B	$tB	#TB	#nB	#�B	"�B	&fB	+�B	.�B	1�B	2�B	3hB	6B	8�B	8�B	8�B	7�B	88B	<6B	>]B	B'B	D3B	F?B	GEB	JXB	MjB	S@B	S@B	S[B	T{B	VmB	VSB	VmB	VmB	V�B	W�B	W
B	U�B	U�B	W?B	W$B	_;B	e�B	i�B	kB	mB	m)B	p�B	s3B	u?B	xB	x8B	z^B	{dB	|jB	|jB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�	B	�B	�/B	�pB	�NB	�@B	�2B	�8B	�eB	�qB	��B	��B	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�	B	�B	�)B	�0B	�<B	�\B	�BB	�bB	�4B	�&B	�aB	֡B	ٚB	�kB	یB	�xB	ܒB	ޞB	ߊB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�FB	�B	�+B	�B	�8B	�$B	�DB	�JB	�qB
 �B
�B	�}B	�BB	�BB
 �B	��B	�<B	�VB	�]B
 iB
 iB
oB
AB
MB
YB
�B

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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
B
B
B
B
9B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
	B
	B
	B
�B
�B
�B
�B
	B
B
CB
/B
B
B
B
;B
;B
 BB
!B
!B
!-B
!HB
"B
"B
"NB
"hB
#:B
# B
"B
# B
# B
"4B
"NB
#TB
$&B
$&B
$ZB
#:B
#:B
# B
# B
$B
$&B
$ZB
$@B
%,B
%,B
%,B
&2B
&2B
(>B
)_B
)_B
*eB
*KB
*KB
*eB
*eB
+kB
+kB
,WB
,=B
,WB
,qB
,WB
,qB
-wB
-wB
-�B
-]B
,WB
-wB
.}B
.�B
-wB
.cB
.}B
/iB
/�B
0�B
1vB
1vB
2�B
2|B
2�B
2�B
2�B
2�B
2|B
2aB
2|B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
>�B
@4B
AB
A�B
BB
B�B
C�B
D�B
D�B
D�B
EB
EB
FB
E�B
E�B
E�B
F�B
F�B
GB
GB
HB
HB
IB
IB
IB
IB
IB
IB
IB
IB
I7B
J=B
J	B
J	B
J	B
J	B
J	B
K)B
KB
KB
K)B
L0B
L0B
LB
K�B
K�B
LB
LB
MB
MB
M6B
MB
MB
MB
MB
MB
MB
MB
MB
N"B
N"B
NB
N"B
N"B
N"B
N"B
N"B
N"B
O(B
OB
O(B
OB
O(B
OB
OB
O(B
OB
O(B
OBB
O(B
P.B
P.B
PHB
P.B
PHB
P.B
QNB
Q4B
Q4B
QNB
QNB
RTB
RoB
S@B
S[B
S[B
S[B
T{B
TFB
UMB
U2B
UMB
UMB
UgB
UgB
U�B
VSB
VmB
WYB
WsB
W�B
X�B
YB
Y�B
Z�B
[�B
[qB
[�B
\�B
\�B
]~B
]~B
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_pB
_pB
_pB
_�B
`�B
`�B
a�B
a�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
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
m�B
n�B
n�B
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
o�B
pB
pB
o�B
o�B
p�B
pB
qB
qB
qB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
tB
tB
tB
tB
s�B
s�B
tB
tB
tB
uB
uB
t�B
uB
uB
uB
vB
vB
vB
vB
vB
u�B
vB
vB
wB
wB
wB
wB
wB
wB
xB
xB
xB
xB
y	B
xB
y$B
y	B
y$B
y$B
y$B
z*B
z*B
z*B
z*B
{JB
{0B
{B
{B
{JB
{0B
{0B
{0B
{0B
|6B
|6B
|6B
|6B
|B
|6B
|B
|B
}"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.42(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705200036232017052000362320170520003623201806221313312018062213133120180622131331201804050715002018040507150020180405071500  JA  ARFMdecpA19c                                                                20170516093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170516003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170516003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170516003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170516003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170516003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170516003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170516003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170516003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170516003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20170516010851                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170516153401  CV  JULD            G�O�G�O�F�=�                JM  ARCAJMQC2.0                                                                 20170519153623  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170519153623  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221500  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041331  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                