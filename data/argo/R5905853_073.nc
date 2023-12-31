CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:35:45Z creation;2022-06-04T17:35:45Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173545  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               IA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�[�[6;�1   @�[´Vx�@/hr� Ĝ�c���$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA@  Aa��A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�ffB�ffB���B���B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�p�@e�@��\@�\A�A9G�AZ�HAz�HA���A���A���A���Ạ�Aܣ�A��A���BQ�BQ�BQ�BQ�B&Q�B.Q�B6Q�B>Q�BFQ�BNQ�BVQ�B^Q�BfQ�BnQ�BvQ�B~Q�B���B��\B��\B�B���B�(�B�(�B�B�(�B�(�B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B���B�(�C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�C!�C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3z�C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�CW�CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D eD �DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D	eD	�D
eD
�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�DeD�D eD �D!eD!�D"eD"�D#eD#�D$eD$�D%eD%�D&eD&�D'eD'�D(eD(�D)eD)�D*eD*�D+eD+�D,eD,�D-eD-�D.k�D.�D/eD/�D0eD0�D1eD1�D2eD2�D3eD3�D4eD4�D5eD5�D6eD6�D7eD7�D8eD8�D9eD9�D:eD:�D;eD;�D<eD<�D=eD=�D>eD>�D?eD?�D@eD@�DAeDA�DBeDB�DCeDC�DDeDD�DEeDE�DFeDF�DGeDG�DHeDH�DIeDI�DJeDJ�DKeDK�DLeDL�DMeDM�DNeDN�DOeDO�DPeDP�DQeDQ�DReDR�DSeDS�DTeDT�DUeDU�DVeDV�DWeDW�DXeDX�DYeDY�DZeDZ�D[eD[�D\eD\�D]eD]�D^eD^�D_eD_�D`eD`�DaeDa�DbeDb�DceDc�DdeDd�DeeDe�DfeDf�DgeDg�DheDh�DieDi�DjeDj�DkeDk�DleDl�DmeDm�DneDn�DoeDo�DpeDp�DqeDq�DreDr�DseDs�DteDt�DueDu�DveDv�DweDw�DxeDx�DyeDy�DzeDz�D{eD{�D|eD|�D}eD}�D~eD~�D^�D�D�2�D�u�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D²�D��D�2�D�r�Dò�D��D�2�D�r�DĲ�D��D�2�D�r�DŲ�D��D�2�D�r�DƲ�D��D�2�D�r�Dǲ�D��D�2�D�r�DȲ�D��D�2�D�r�Dɲ�D��D�2�D�r�Dʲ�D��D�2�D�r�D˲�D��D�2�D�r�D̲�D��D�2�D�r�DͲ�D��D�2�D�r�Dβ�D��D�2�D�r�Dϲ�D��D�2�D�r�Dв�D��D�2�D�r�DѲ�D��D�2�D�r�DҲ�D��D�2�D�r�DӲ�D��D�5�D�r�DԲ�D��D�2�D�r�Dղ�D��D�2�D�r�Dֲ�D��D�2�D�r�Dײ�D��D�2�D�r�Dز�D��D�2�D�r�Dٲ�D��D�2�D�r�Dڲ�D��D�2�D�r�D۲�D��D�2�D�r�Dܲ�D��D�2�D�r�Dݲ�D��D�2�D�r�D޲�D��D�2�D�r�D߲�D��D�2�D�r�DಏD��D�2�D�r�D᲏D��D�2�D�r�DⲏD��D�2�D�r�D㲏D��D�2�D�r�D䲏D��D�2�D�r�D岏D��D�2�D�r�D沏D��D�2�D�r�D粏D��D�2�D�r�D貏D��D�2�D�r�D鲏D��D�2�D�r�D겏D��D�2�D�r�D벏D��D�2�D�r�D첏D��D�2�D�r�D���D��D�2�D�r�DD��D�2�D�r�DﲏD��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D�D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���D��D�2�D�r�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA�"A�$�Aǡ�AǄ�A�w�A�k�A�c�A�_A�[�A�ZQA�XyA�XA�Z�A�[�A�[�A�\�A�]�A�^5A�_A�`A�`�A�a�A�d�A�g�A�lWA�m]A�n�A�s�A�{JA�|�A�{A��A�P}A���A�!�A�;�A�+�A�2aA���A��%A���A�$A�Y�A���A���A���A�A��aA��$A�.}A���A�RTA���A�+kA���A�ZQA�cTA���A�v�A�uA���A���A�0�A�  A���A�?HA��(A���A�B�A�kA�=<A��A���A|C-Ay�4Aw�NAs`BAo?�Aj�tAh��AgL�AcH�A]�PA\�+A\�AX�,ATU2AP�]AL�6AF�oAB-�A@�cA@xA?�A?ݘA?4�A=\�A:|�A9@�A8�qA833A7~(A633A5�A4xA3��A3��A4A A5j�A6�A5�'A4� A4A A3��A36�A3�A1x�A0�A0��A0g8A/��A,�:A*tTA)j�A)A(3�A'}VA'�fA(��A)�A* �A(�-A'jA'�A&�.A%�	A$_�A$.�A#ݘA#�A"c�A!�aA!��A!{A ��A A��A�A�]A�gA�A�7A�6A�Au�A��AG�A��A|�Ad�A�gA��A��A��A_AS�A:�A��A�AN<A:�Au�A��A'�A�#A,�A�AYA�Ae�AqA��Ah
A=A
+A	�fA	:�A�KA��A$AqA�A`�A&A��A~�A8�AĜA@OA�A�tA��A�fA*0A�A��A�A�A}�A��A��A��A��A\)AuA ��A ��A �:A 9�@��@�	@��w@��{@�S�@�'�@���@�p;@��@��)@�]d@�b@���@�"�@��@���@��K@�a�@�S@���@� �@��@��H@�S@�-@�@�G@��@�O@���@�ϫ@��z@��	@�1�@�=q@�M�@��z@�@O@��5@�Ft@�|@�"�@���@���@ݲ-@�=@��f@�M�@��d@��U@�B[@��@٪�@�+@�S@���@��]@�Vm@��@�خ@�@O@Ԝx@�3�@��@Ӣ�@�A�@�(@�֡@�*�@��@ѷ�@�5�@�N�@ϻ0@ψf@�}�@�8@��@���@��2@ξ@�4n@���@��m@͹�@��y@�,=@ˌ~@�iD@�F@�!�@�ی@ʙ1@�|�@�h
@�Z@�@�@ɱ[@�X�@�,�@�S@��H@Ȣ4@�@Ǻ^@�Y�@��s@ƂA@�Z�@Ź�@�e�@�5�@��@���@��,@Ķ�@�_�@��@à�@�{J@�\)@�҉@�u%@�Xy@�"h@�X�@��@���@�q�@�m]@�Ɇ@��u@�)�@��6@��"@��5@�l"@��N@�j@�L�@��@���@�K^@�!�@���@�O@��@�g8@��r@�4@��s@��@�Q@�M@��@��4@�Z�@���@�� @��N@��@���@�W?@��@��_@�B[@�?�@�:*@�x@���@��@�@�҉@�I�@��@���@�P�@��@���@��@�u�@�E�@�1'@���@�@��v@��@���@��h@��_@��r@���@�~(@�bN@�6@��@���@�� @���@�hs@�+@��K@���@�u%@��0@�Q�@�@@���@��z@� �@���@��C@���@�&@��|@��9@��@��\@�{�@�*�@���@��[@���@�9�@���@�
�@��6@�o @��@�	l@���@��6@��@���@��@�u�@�W?@��@�ȴ@�q�@��j@�hs@��P@��D@�Q�@���@��@�
=@���@�e�@���@�_p@��L@�m�@�%�@�b�@�(�@��@��c@�|�@�z�@�W�@���@�o @�O�@�q@�z�@�:*@�%�@���@��'@�2a@���@��@��_@�Ta@��@���@���@�Z�@�1�@� \@��8@��<@��z@��@�GE@��@���@���@��4@��	@���@���@�"h@��&@��z@�x�@�N<@�0�@�C@��@���@��c@�҉@���@�b@��;@���@�]�@��@��@��c@���@���@��F@���@�~(@�n�@�N�@�	@��@��@��K@���@�n/@�/�@��B@�Xy@��H@��@���@�X�@�
=@���@���@��h@�|�@�'R@���@���@���@�ϫ@��3@���@�s@�&�@���@��\@�N�@�>B@�:*@�	�@���@���@�p�@�e�@�A�@�!�@��@��@���@�Z@��@��@��$@�J�@��@��|@��@���@��h@��}@���@��_@��D@�_�@�Ft@�@��@�v`@�A @���@��@��9@�}V@�Q@�>B@�+k@�@��@RT@~��@}�@}^�@}7L@}4@}#�@|�@{�@{�W@{��@{��@{��@{g�@z��@z�@y�S@y;@x�D@xD�@x@w�Q@w_p@w.I@v�B@v3�@uN<@s�Q@r�F@rYK@q��@p��@p,=@oiD@n�@n��@na|@n#:@nu@m�Z@m��@m�H@m��@m?}@l��@k�@k�@kiD@j�@j��@j��@j��@j�L@j��@j��@j�@i��@io @i0�@h��@hw�@g�@@g>�@f��@e�)@e�H@eq@d�p@dr�@dK^@d@c�f@c8@b��@a�@a7L@`��@`�[@`y>@_�
@_��@_�@^�<@]�@]�@]X@\�@\��@\Xy@\�@[�}@[�4@[@O@[,�@[�@Z��@Z~�@Y�@Y�S@X�f@X��@X|�@XD�@W�6@W�4@W33@V�'@V$�@Us�@T��@Toi@T_@T1'@S��@S��@SY@R͟@Q�#@Qe,@Q+@P�@P�$@P��@P�@P�.@P�@P[�@P�@O�[@OZ�@N�!@N6�@M�@Ma�@LĜ@LK^@Kخ@K�V@Kqv@Kqv@Kn/@K\)@K.I@J��@J�@Jl�@Jh
@J^5@JV@JM�@J.�@J�@Ju@I�T@I�@IT�@I4@H��@H�@G��@Gخ@G��@G\)@F��@FV@F{@E��@Em]@E�@D��@D�[@D��@D�@C�[@C!-@B��@Bi�@B{@A�h@A@@@��@@7@?n/@>҉@=?}@=@=�@<�U@<��@<�@;�@:��@:?@:4@9�>@9�#@9�C@9�@9s�@9O�@9 \@8��@8Z@7�@7]�@7U�@7A�@7;d@74�@7�@6�@6�X@6�@6d�@6R�@6	@5��@5�@5&�@4�	@4֡@4m�@4Xy@4Ft@4~@4x@3�0@3_p@3J#@3"�@2� @1�@1��@1�-@1|@1O�@1/@0��@0�U@0�@0r�@0bN@0Z@0K^@0*�@/�@/�
@/�$@.��@-�-@-%@,��@,�I@,U2@,/�@+�@+ƨ@+��@+~�@+o�@+P�@+�@*��@*ߤ@*҉@*��@*a|@*R�@*Ov@*3�@*_@)��@)�@)��@(��@(u�@($@(�@(M@(b@'�+@'�@@'t�@'Mj@&�8@&�@%��@%f�@%!�@$��@$'R@$�@#��@#�a@#�*@#��@#n/@#�@"�m@"�+@"^5@"1�@"�@"�@"�@"_@!��@!8�@ N�@ *�@ �@�Q@��@{J@6z@�B@W�@M�@��@�M@�4@`�@7�@7@�@�@��@A�@q�@��@�d@�X@��@Y�@�@q@<�@~@�r@�A@� @y�@O@A�@�8@��@� @�r@s�@\�@5?@	@�@ѷ@r�@	�@�W@�@�g@�[@��@t�@=@;d@9�@��@�@�@�@��@�@��@YK@8�@�@�@{@{@�@�@��@�@�@J�@@@��@u�@m�@m�@h�@[�@H@ �@x@�@�r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA�"A�$�Aǡ�AǄ�A�w�A�k�A�c�A�_A�[�A�ZQA�XyA�XA�Z�A�[�A�[�A�\�A�]�A�^5A�_A�`A�`�A�a�A�d�A�g�A�lWA�m]A�n�A�s�A�{JA�|�A�{A��A�P}A���A�!�A�;�A�+�A�2aA���A��%A���A�$A�Y�A���A���A���A�A��aA��$A�.}A���A�RTA���A�+kA���A�ZQA�cTA���A�v�A�uA���A���A�0�A�  A���A�?HA��(A���A�B�A�kA�=<A��A���A|C-Ay�4Aw�NAs`BAo?�Aj�tAh��AgL�AcH�A]�PA\�+A\�AX�,ATU2AP�]AL�6AF�oAB-�A@�cA@xA?�A?ݘA?4�A=\�A:|�A9@�A8�qA833A7~(A633A5�A4xA3��A3��A4A A5j�A6�A5�'A4� A4A A3��A36�A3�A1x�A0�A0��A0g8A/��A,�:A*tTA)j�A)A(3�A'}VA'�fA(��A)�A* �A(�-A'jA'�A&�.A%�	A$_�A$.�A#ݘA#�A"c�A!�aA!��A!{A ��A A��A�A�]A�gA�A�7A�6A�Au�A��AG�A��A|�Ad�A�gA��A��A��A_AS�A:�A��A�AN<A:�Au�A��A'�A�#A,�A�AYA�Ae�AqA��Ah
A=A
+A	�fA	:�A�KA��A$AqA�A`�A&A��A~�A8�AĜA@OA�A�tA��A�fA*0A�A��A�A�A}�A��A��A��A��A\)AuA ��A ��A �:A 9�@��@�	@��w@��{@�S�@�'�@���@�p;@��@��)@�]d@�b@���@�"�@��@���@��K@�a�@�S@���@� �@��@��H@�S@�-@�@�G@��@�O@���@�ϫ@��z@��	@�1�@�=q@�M�@��z@�@O@��5@�Ft@�|@�"�@���@���@ݲ-@�=@��f@�M�@��d@��U@�B[@��@٪�@�+@�S@���@��]@�Vm@��@�خ@�@O@Ԝx@�3�@��@Ӣ�@�A�@�(@�֡@�*�@��@ѷ�@�5�@�N�@ϻ0@ψf@�}�@�8@��@���@��2@ξ@�4n@���@��m@͹�@��y@�,=@ˌ~@�iD@�F@�!�@�ی@ʙ1@�|�@�h
@�Z@�@�@ɱ[@�X�@�,�@�S@��H@Ȣ4@�@Ǻ^@�Y�@��s@ƂA@�Z�@Ź�@�e�@�5�@��@���@��,@Ķ�@�_�@��@à�@�{J@�\)@�҉@�u%@�Xy@�"h@�X�@��@���@�q�@�m]@�Ɇ@��u@�)�@��6@��"@��5@�l"@��N@�j@�L�@��@���@�K^@�!�@���@�O@��@�g8@��r@�4@��s@��@�Q@�M@��@��4@�Z�@���@�� @��N@��@���@�W?@��@��_@�B[@�?�@�:*@�x@���@��@�@�҉@�I�@��@���@�P�@��@���@��@�u�@�E�@�1'@���@�@��v@��@���@��h@��_@��r@���@�~(@�bN@�6@��@���@�� @���@�hs@�+@��K@���@�u%@��0@�Q�@�@@���@��z@� �@���@��C@���@�&@��|@��9@��@��\@�{�@�*�@���@��[@���@�9�@���@�
�@��6@�o @��@�	l@���@��6@��@���@��@�u�@�W?@��@�ȴ@�q�@��j@�hs@��P@��D@�Q�@���@��@�
=@���@�e�@���@�_p@��L@�m�@�%�@�b�@�(�@��@��c@�|�@�z�@�W�@���@�o @�O�@�q@�z�@�:*@�%�@���@��'@�2a@���@��@��_@�Ta@��@���@���@�Z�@�1�@� \@��8@��<@��z@��@�GE@��@���@���@��4@��	@���@���@�"h@��&@��z@�x�@�N<@�0�@�C@��@���@��c@�҉@���@�b@��;@���@�]�@��@��@��c@���@���@��F@���@�~(@�n�@�N�@�	@��@��@��K@���@�n/@�/�@��B@�Xy@��H@��@���@�X�@�
=@���@���@��h@�|�@�'R@���@���@���@�ϫ@��3@���@�s@�&�@���@��\@�N�@�>B@�:*@�	�@���@���@�p�@�e�@�A�@�!�@��@��@���@�Z@��@��@��$@�J�@��@��|@��@���@��h@��}@���@��_@��D@�_�@�Ft@�@��@�v`@�A @���@��@��9@�}V@�Q@�>B@�+k@�@��@RT@~��@}�@}^�@}7L@}4@}#�@|�@{�@{�W@{��@{��@{��@{g�@z��@z�@y�S@y;@x�D@xD�@x@w�Q@w_p@w.I@v�B@v3�@uN<@s�Q@r�F@rYK@q��@p��@p,=@oiD@n�@n��@na|@n#:@nu@m�Z@m��@m�H@m��@m?}@l��@k�@k�@kiD@j�@j��@j��@j��@j�L@j��@j��@j�@i��@io @i0�@h��@hw�@g�@@g>�@f��@e�)@e�H@eq@d�p@dr�@dK^@d@c�f@c8@b��@a�@a7L@`��@`�[@`y>@_�
@_��@_�@^�<@]�@]�@]X@\�@\��@\Xy@\�@[�}@[�4@[@O@[,�@[�@Z��@Z~�@Y�@Y�S@X�f@X��@X|�@XD�@W�6@W�4@W33@V�'@V$�@Us�@T��@Toi@T_@T1'@S��@S��@SY@R͟@Q�#@Qe,@Q+@P�@P�$@P��@P�@P�.@P�@P[�@P�@O�[@OZ�@N�!@N6�@M�@Ma�@LĜ@LK^@Kخ@K�V@Kqv@Kqv@Kn/@K\)@K.I@J��@J�@Jl�@Jh
@J^5@JV@JM�@J.�@J�@Ju@I�T@I�@IT�@I4@H��@H�@G��@Gخ@G��@G\)@F��@FV@F{@E��@Em]@E�@D��@D�[@D��@D�@C�[@C!-@B��@Bi�@B{@A�h@A@@@��@@7@?n/@>҉@=?}@=@=�@<�U@<��@<�@;�@:��@:?@:4@9�>@9�#@9�C@9�@9s�@9O�@9 \@8��@8Z@7�@7]�@7U�@7A�@7;d@74�@7�@6�@6�X@6�@6d�@6R�@6	@5��@5�@5&�@4�	@4֡@4m�@4Xy@4Ft@4~@4x@3�0@3_p@3J#@3"�@2� @1�@1��@1�-@1|@1O�@1/@0��@0�U@0�@0r�@0bN@0Z@0K^@0*�@/�@/�
@/�$@.��@-�-@-%@,��@,�I@,U2@,/�@+�@+ƨ@+��@+~�@+o�@+P�@+�@*��@*ߤ@*҉@*��@*a|@*R�@*Ov@*3�@*_@)��@)�@)��@(��@(u�@($@(�@(M@(b@'�+@'�@@'t�@'Mj@&�8@&�@%��@%f�@%!�@$��@$'R@$�@#��@#�a@#�*@#��@#n/@#�@"�m@"�+@"^5@"1�@"�@"�@"�@"_@!��@!8�@ N�@ *�@ �@�Q@��@{J@6z@�B@W�@M�@��@�M@�4@`�@7�@7@�@�@��@A�@q�@��@�d@�X@��@Y�@�@q@<�@~@�r@�A@� @y�@O@A�@�8@��@� @�r@s�@\�@5?@	@�@ѷ@r�@	�@�W@�@�g@�[@��@t�@=@;d@9�@��@�@�@�@��@�@��@YK@8�@�@�@{@{@�@�@��@�@�@J�@@@��@u�@m�@m�@h�@[�@H@ �@x@�@�r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�BsB1BeB�BB�B+B�BeB�B7B�B�B�B	B#B#BWBqB�B�B�B BB"4B$B&�B)�B*�B*�B=<Bl�B	aB
9�B
B�B
��BB�\B��B��B��B�6BΊB��B�B��B��BSBeB�BFBvBxB�B�B�B�(B�BڠB�XB��B�HBm�BO�B3�B�B
�B
�lB
��B
�7B
s3B
C�B	��B	�B	�gB	�wB	��B	|jB	k�B	abB	KB	,�B	$�B	B	B��B�B��B�'B�CB��B��B��B��B��B��B�B�B�LB�6B��B�_B��B�*B	6B	 B	8RB	_�B	z�B	��B	��B	�B	�uB	��B	�CB	�HB	�mB	�B	�RB	��B	�]B	��B	��B	�=B	��B	�DB	��B	�"B	��B	��B
+B
�B
�B
B
�B
	B
	RB
�B
_B
B
{B
�B
�B

�B
xB
B
B
	B
�B
�B	��B	�LB
�B
B
 �B
 �B
  B	�wB	�VB	��B
 B
gB
�B
�B
B

rB
^B
�B
�B
MB	��B	�<B	�qB
'B
B
�B
	7B
�B
 B	��B	��B	�B	�B	��B	�B	��B
oB	��B	��B	�0B	��B	��B	��B	�*B	��B	��B	�rB	��B	��B	�B	�.B
B
�B
�B
dB
	�B
^B

�B
	B
1B
�B
�B
?B
�B
B
�B
�B
�B
'B	��B	�.B	�]B	��B	�VB	��B	��B	��B	�B	��B	��B	�lB	��B	��B	��B	�MB	�B	��B	��B	�?B	�9B	�MB	��B	�B	��B	�mB	��B	�B	�xB	��B	�SB	՛B	�1B	�kB	�YB	�$B	ؓB	�eB	��B	�7B	ٴB	ٴB	�7B	��B	�7B	��B	�)B	�/B	��B	��B	��B	�B	ߤB	�B	� B	�TB	�B	�nB	��B	�B	� B	�B	��B	�|B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�,B	�`B	�B	�mB	�8B	�mB	�B	�B	�
B	��B	�B	�B	��B	��B	�eB	�B	�B	�B	��B	��B	��B	�OB	�B	�;B	�AB	�vB	��B	�B	�AB	��B	�B	��B	�aB	�B	�-B	�B	��B	�B	�B	�FB	��B	��B	��B	��B	�zB	��B	��B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�"B	�VB	��B	��B	�jB	��B	��B	�0B	�jB	��B	�]B	�BB	��B	��B	�wB	�wB	��B	�qB	�VB	�}B	��B	��B	�PB
 4B
�B
�B
 �B
oB
;B
 �B
 �B
'B
AB
�B
YB
tB
B

	B

�B
DB
�B
dB
�B
dB
�B
�B
�B
6B
"B
�B
"B
B
<B
VB
VB
VB
VB
pB
�B
�B
B
�B
B
B
�B
�B
HB
.B
 B
 B
TB
:B
�B
�B
�B
�B
�B
 B
�B
�B
uB
�B
�B
B
�B
�B
{B
{B
�B
�B
�B
�B
aB
,B
,B
aB
B
2B
gB
gB
�B
B
B
9B
�B
?B
sB
sB
�B
�B
EB
�B
�B
�B
B
B
yB
B
eB
�B
�B
�B
B
QB
�B
QB
�B
	B
�B
�B
�B
�B
qB
qB
B
�B
]B
)B
�B
�B
�B
�B
IB
�B
B
B
jB
jB
OB
5B
5B
B
OB
�B
!HB
"�B
#B
"�B
#�B
#nB
#nB
$@B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%,B
$�B
$�B
%�B
&LB
&2B
&fB
&LB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'B
&�B
'B
'8B
'RB
(sB
-)B
-wB
-�B
.B
.IB
./B
.cB
.}B
.�B
/B
/iB
/�B
/�B
0UB
0;B
0B
0oB
0�B
0�B
1B
0�B
1B
0�B
1AB
1[B
1vB
1�B
2B
2aB
2�B
3B
33B
3MB
3�B
3�B
3�B
3�B
3hB
3�B
3�B
3�B
3�B
4B
4TB
4�B
4�B
5?B
5%B
5?B
5�B
5�B
5�B
5�B
5�B
5�B
6`B
6`B
72B
7�B
7B
7B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8�B
8�B
8�B
9	B
9XB
9�B
:B
9�B
9�B
:*B
:^B
:xB
:B
:^B
:B
:xB
:�B
:�B
:�B
;B
;B
;�B
;�B
;�B
;�B
;�B
<6B
<6B
<�B
<�B
=B
=�B
=�B
=�B
>(B
>B
>B
>B
>BB
>�B
>�B
>�B
?cB
?B
@ B
?�B
@�B
A B
A;B
A�B
A�B
B[B
BAB
B�B
B�B
B�B
CGB
DB
D�B
D�B
D�B
D�B
EB
E9B
EmB
E�B
E�B
ESB
ESB
ESB
ESB
ESB
ESB
EmB
E�B
E�B
E�B
E�B
FB
E�B
FYB
F%B
F�B
F�B
F�B
F�B
GB
GEB
GEB
G�B
G�B
H�B
IB
IB
IB
IRB
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K)B
K)B
KDB
K)B
KDB
KxB
K�B
L0B
LdB
MPB
M�B
NVB
N<B
O\B
O�B
P.B
PHB
P�B
P}B
P�B
P�B
P�B
Q B
QNB
QhB
QhB
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R:B
RTB
SB
S@B
SuB
S[B
S@B
S�B
T,B
TaB
TaB
T�B
T�B
UMB
U2B
UB
T�B
U�B
U�B
VB
VB
VB
V9B
V�B
VSB
VmB
V�B
V�B
V�B
V�B
VSB
VmB
V�B
VmB
V�B
WYB
XB
XEB
X�B
X�B
X�B
YKB
YeB
Y�B
Y�B
Y�B
ZB
Z�B
[�B
\B
\)B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]�B
]�B
^OB
^jB
^�B
^�B
^�B
_!B
_!B
_!B
_pB
_�B
_�B
_�B
_�B
`\B
`\B
`\B
`vB
`�B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
bB
b�B
b�B
b�B
cB
c:B
c:B
cnB
cTB
c�B
c�B
c�B
c�B
dZB
d&B
dB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
fB
fLB
f2B
ffB
fB
ffB
f�B
f�B
gB
g8B
g8B
h�B
h�B
i*B
i�B
j0B
j0B
jB
jeB
jKB
jKB
j�B
kB
kB
k6B
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
mB
m)B
mB
m]B
m�B
m�B
m�B
nB
n�B
oiB
o�B
o�B
pB
pB
o�B
pB
p�B
q�B
q�B
rGB
rGB
raB
r|B
r�B
sMB
shB
shB
s�B
s�B
s�B
tB
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
vFB
v+B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
v�B
wfB
wLB
wLB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
y	B
y>B
y$B
y$B
y$B
y>B
y>B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�BsB1BeB�BB�B+B�BeB�B7B�B�B�B	B#B#BWBqB�B�B�B BB"4B$B&�B)�B*�B*�B=<Bl�B	aB
9�B
B�B
��BB�\B��B��B��B�6BΊB��B�B��B��BSBeB�BFBvBxB�B�B�B�(B�BڠB�XB��B�HBm�BO�B3�B�B
�B
�lB
��B
�7B
s3B
C�B	��B	�B	�gB	�wB	��B	|jB	k�B	abB	KB	,�B	$�B	B	B��B�B��B�'B�CB��B��B��B��B��B��B�B�B�LB�6B��B�_B��B�*B	6B	 B	8RB	_�B	z�B	��B	��B	�B	�uB	��B	�CB	�HB	�mB	�B	�RB	��B	�]B	��B	��B	�=B	��B	�DB	��B	�"B	��B	��B
+B
�B
�B
B
�B
	B
	RB
�B
_B
B
{B
�B
�B

�B
xB
B
B
	B
�B
�B	��B	�LB
�B
B
 �B
 �B
  B	�wB	�VB	��B
 B
gB
�B
�B
B

rB
^B
�B
�B
MB	��B	�<B	�qB
'B
B
�B
	7B
�B
 B	��B	��B	�B	�B	��B	�B	��B
oB	��B	��B	�0B	��B	��B	��B	�*B	��B	��B	�rB	��B	��B	�B	�.B
B
�B
�B
dB
	�B
^B

�B
	B
1B
�B
�B
?B
�B
B
�B
�B
�B
'B	��B	�.B	�]B	��B	�VB	��B	��B	��B	�B	��B	��B	�lB	��B	��B	��B	�MB	�B	��B	��B	�?B	�9B	�MB	��B	�B	��B	�mB	��B	�B	�xB	��B	�SB	՛B	�1B	�kB	�YB	�$B	ؓB	�eB	��B	�7B	ٴB	ٴB	�7B	��B	�7B	��B	�)B	�/B	��B	��B	��B	�B	ߤB	�B	� B	�TB	�B	�nB	��B	�B	� B	�B	��B	�|B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	�,B	�`B	�B	�mB	�8B	�mB	�B	�B	�
B	��B	�B	�B	��B	��B	�eB	�B	�B	�B	��B	��B	��B	�OB	�B	�;B	�AB	�vB	��B	�B	�AB	��B	�B	��B	�aB	�B	�-B	�B	��B	�B	�B	�FB	��B	��B	��B	��B	�zB	��B	��B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�"B	�VB	��B	��B	�jB	��B	��B	�0B	�jB	��B	�]B	�BB	��B	��B	�wB	�wB	��B	�qB	�VB	�}B	��B	��B	�PB
 4B
�B
�B
 �B
oB
;B
 �B
 �B
'B
AB
�B
YB
tB
B

	B

�B
DB
�B
dB
�B
dB
�B
�B
�B
6B
"B
�B
"B
B
<B
VB
VB
VB
VB
pB
�B
�B
B
�B
B
B
�B
�B
HB
.B
 B
 B
TB
:B
�B
�B
�B
�B
�B
 B
�B
�B
uB
�B
�B
B
�B
�B
{B
{B
�B
�B
�B
�B
aB
,B
,B
aB
B
2B
gB
gB
�B
B
B
9B
�B
?B
sB
sB
�B
�B
EB
�B
�B
�B
B
B
yB
B
eB
�B
�B
�B
B
QB
�B
QB
�B
	B
�B
�B
�B
�B
qB
qB
B
�B
]B
)B
�B
�B
�B
�B
IB
�B
B
B
jB
jB
OB
5B
5B
B
OB
�B
!HB
"�B
#B
"�B
#�B
#nB
#nB
$@B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%,B
$�B
$�B
%�B
&LB
&2B
&fB
&LB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'mB
'B
&�B
'B
'8B
'RB
(sB
-)B
-wB
-�B
.B
.IB
./B
.cB
.}B
.�B
/B
/iB
/�B
/�B
0UB
0;B
0B
0oB
0�B
0�B
1B
0�B
1B
0�B
1AB
1[B
1vB
1�B
2B
2aB
2�B
3B
33B
3MB
3�B
3�B
3�B
3�B
3hB
3�B
3�B
3�B
3�B
4B
4TB
4�B
4�B
5?B
5%B
5?B
5�B
5�B
5�B
5�B
5�B
5�B
6`B
6`B
72B
7�B
7B
7B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8�B
8�B
8�B
9	B
9XB
9�B
:B
9�B
9�B
:*B
:^B
:xB
:B
:^B
:B
:xB
:�B
:�B
:�B
;B
;B
;�B
;�B
;�B
;�B
;�B
<6B
<6B
<�B
<�B
=B
=�B
=�B
=�B
>(B
>B
>B
>B
>BB
>�B
>�B
>�B
?cB
?B
@ B
?�B
@�B
A B
A;B
A�B
A�B
B[B
BAB
B�B
B�B
B�B
CGB
DB
D�B
D�B
D�B
D�B
EB
E9B
EmB
E�B
E�B
ESB
ESB
ESB
ESB
ESB
ESB
EmB
E�B
E�B
E�B
E�B
FB
E�B
FYB
F%B
F�B
F�B
F�B
F�B
GB
GEB
GEB
G�B
G�B
H�B
IB
IB
IB
IRB
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K)B
K)B
KDB
K)B
KDB
KxB
K�B
L0B
LdB
MPB
M�B
NVB
N<B
O\B
O�B
P.B
PHB
P�B
P}B
P�B
P�B
P�B
Q B
QNB
QhB
QhB
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R:B
RTB
SB
S@B
SuB
S[B
S@B
S�B
T,B
TaB
TaB
T�B
T�B
UMB
U2B
UB
T�B
U�B
U�B
VB
VB
VB
V9B
V�B
VSB
VmB
V�B
V�B
V�B
V�B
VSB
VmB
V�B
VmB
V�B
WYB
XB
XEB
X�B
X�B
X�B
YKB
YeB
Y�B
Y�B
Y�B
ZB
Z�B
[�B
\B
\)B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]�B
]�B
^OB
^jB
^�B
^�B
^�B
_!B
_!B
_!B
_pB
_�B
_�B
_�B
_�B
`\B
`\B
`\B
`vB
`�B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
bB
b�B
b�B
b�B
cB
c:B
c:B
cnB
cTB
c�B
c�B
c�B
c�B
dZB
d&B
dB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
fB
fLB
f2B
ffB
fB
ffB
f�B
f�B
gB
g8B
g8B
h�B
h�B
i*B
i�B
j0B
j0B
jB
jeB
jKB
jKB
j�B
kB
kB
k6B
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
mB
m)B
mB
m]B
m�B
m�B
m�B
nB
n�B
oiB
o�B
o�B
pB
pB
o�B
pB
p�B
q�B
q�B
rGB
rGB
raB
r|B
r�B
sMB
shB
shB
s�B
s�B
s�B
tB
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
vFB
v+B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
wB
v�B
wfB
wLB
wLB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
xB
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
y	B
y>B
y$B
y$B
y$B
y>B
y>B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104914  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173545  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173545  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173545                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023553  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023553  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                