CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-05-15T09:00:47Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200515090047  20200515090047  5906096 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               &A   AO  7902                            2B  A   NAVIS_A                         1010                            170425                          863 @���;�1   @��K��@*��`A�7�c���+1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      &A   A   A   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33BЙ�Bә�B���B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D3D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�3D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�33@�  A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB���B���B�  B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS�4CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG��DHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDw�fDxffDx�fDyffDy�fDzffDz�fD{ffD{�fD|ffD|�fD}ffD}�fD~ffD~�fDffD�fD�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D�� D�33D�s3D��3D��3D�33D�vfD��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�vfD³3D��3D�33D�s3Dó3D��3D�33D�s3Dĳ3D��3D�33D�s3Dų3D��3D�33D�s3DƳ3D��3D�33D�s3Dǳ3D��3D�33D�s3Dȳ3D��3D�33D�s3Dɳ3D��3D�33D�s3Dʳ3D��3D�33D�s3D˰ D�� D�33D�s3D̳3D��3D�33D�s3Dͳ3D��3D�33D�s3Dγ3D��3D�33D�s3Dϳ3D��3D�33D�s3Dг3D��3D�33D�s3Dѳ3D��3D�33D�s3Dҳ3D��3D�33D�s3Dӳ3D��3D�33D�s3DԳ3D��3D�33D�s3Dճ3D��3D�33D�s3Dֳ3D��3D�33D�s3D׳3D��3D�33D�s3Dس3D��3D�33D�s3Dٳ3D��3D�33D�s3Dڳ3D��3D�33D�s3D۳3D��3D�33D�s3Dܳ3D��3D�33D�s3Dݳ3D��3D�33D�s3D޳3D��fD�33D�s3D߳3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�0 D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D��3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�A�A�A�A�A�A�A�A�%A�%A�%A�%A���A���Aѧ�Aѡ�Aџ�Aѝ�Aћ�Aћ�Aљ�Aї�Aї�Aї�Aї�AѓuAёhAёhAя\AэPAыDAщ7AхAсA�z�A�p�A�l�A�jA�hsA�ffA�bNA�^5A�ZA�ZA�VA�O�A�E�A�/A���A�%A�z�A�p�A��HA��A�S�A��A��PA��;A��A��#A��A�ffA���A�`BA��A�ȴA��A�5?A�XA��9A�;dA��yA�A�dZA�A��!A���A��PA�(�A��RA��A�$�A��A}��A|�Ay�wAtVAp�jAi�mAcAb  A]�7AX�AVJAT��AR�/AP��AO+AKVAJI�AI`BAF��AE/AC��AA�#A=��A<M�A?�AE�AF��AD�+A@bNA4M�A2=qA0�\A,z�A+�A*�`A*JA*�A)�A)�7A)hsA)��A)7LA(bNA'�A'�A&-A&I�A&M�A&M�A%�mA%l�A%33A$��A$�A#O�A"�A!�TA!K�A ��A (�A $�A��A M�A 1AK�A��AQ�AA��A�7AhsAffAQ�AVA$�A�AdZAVA�HA�+AJAt�AG�AĜA9XA��A��A�A��AC�A�9AZA^5A9XA-A�A��A\)A
=A�A�DA�AA�FAO�A��A1'A1A�A��A�wA�A?}A7LAVA��AA�;Al�A%A�A��AVA$�AJA��A
�9A
�A
A	�A	�-A	hsA	/A�Ar�AbNAVA=qA�A�;A|�AO�A��A�A�DA �A��A`BAA��AjA{A�mA�PA\)A?}AVAĜAZA �A�mA��AhsA/A%AA ��A �A ��A �uA r�A M�A $�A J@���@�\)@�"�@��R@�M�@�5?@�J@��h@��j@�Z@��w@�
=@���@��\@�M�@���@��@���@�\)@��!@�@�O�@��@���@���@��@�;d@���@�-@�^@�X@�bN@�S�@�o@��H@�R@@�~�@�$�@�p�@웦@��@���@�dZ@�;d@�ff@�-@�?}@��@�D@���@��@�@��@���@�D@���@�@���@�\@�^5@��#@�@�&�@�Ĝ@�(�@�dZ@�+@ޟ�@���@ݩ�@��`@�bN@��m@�o@�$�@�O�@ؼj@�Q�@���@�C�@�^5@Չ7@Ԭ@�b@�|�@��@җ�@���@�x�@�hs@�G�@��`@�Z@�33@Χ�@�~�@�5?@�@�hs@�/@���@̛�@�1'@��@˝�@�C�@���@�n�@�{@���@�p�@ț�@�  @Ǯ@�K�@Ɵ�@��@őh@�/@�%@Ĭ@�1'@î@��H@�v�@�=q@��^@�x�@�&�@��/@�bN@�b@��w@�t�@�+@��@��\@�~�@�ff@�V@�$�@��#@�O�@��@��u@�C�@�~�@�J@���@�X@��@�Ĝ@��u@�A�@��@��;@���@��+@�5?@���@�&�@�I�@��;@�|�@�K�@�"�@��@���@��@���@�x�@�`B@�O�@�7L@�I�@��w@��@�;d@��R@�v�@�M�@���@��@���@��9@��@��P@�S�@�33@�
=@��y@���@�n�@��#@�G�@�&�@���@�1'@��
@���@�;d@���@��y@���@���@�^5@�@��#@��-@�x�@�/@��`@���@��u@�I�@��@���@�\)@�"�@���@�v�@�=q@�{@���@���@�p�@�O�@���@�Z@��@��w@�dZ@���@�n�@��-@�`B@��@��@��@���@��P@��@�|�@�l�@�;d@�
=@��@���@���@�/@��@�V@���@��j@���@�r�@�Z@�9X@��m@�dZ@�+@���@���@�^5@��-@�O�@��@���@���@�j@�(�@���@�|�@�33@�o@���@���@�n�@�{@��-@��@���@��D@�j@�Q�@���@�l�@�o@��@��!@�n�@�J@��#@��-@�?}@��u@��@��
@��P@�S�@��@�@��y@���@���@�-@��@�V@��@�r�@�Q�@��@���@��F@��P@�l�@�\)@�C�@�o@���@�E�@�$�@���@��@��^@�hs@��@��j@��u@�z�@�Q�@�b@��
@�t�@�+@��@���@���@��+@�^5@��@��-@�x�@�O�@�V@�%@��`@��j@���@�(�@�w@K�@+@
=@~��@~V@}�T@}��@}O�@}�@|z�@{�m@{�@{C�@{o@z�!@zn�@z^5@zM�@z=q@zJ@y��@xĜ@x�@xQ�@w��@w
=@vv�@vV@u�@t�/@t(�@s�F@s33@r~�@r�@q��@q�^@qG�@p�9@pA�@o;d@o�@o�@n�y@m@m?}@mV@l�D@l9X@k�
@kdZ@j�\@i��@ix�@iX@h�`@h�@hbN@h1'@g�P@f��@e�h@ep�@e?}@d��@d�@d��@d�D@d9X@c��@cƨ@c�F@c�@c33@b�@a�7@aX@a&�@`Ĝ@`�@_�;@_
=@^�@^V@]V@\�@\I�@[�m@[C�@[33@Z�H@Z�\@Y��@X�@W�w@V��@V{@U�h@U?}@T�@T�D@Sƨ@R�@R�H@R��@R^5@R-@Q�#@P�u@P �@O�@O�@O\)@N�y@N�R@N��@N��@N$�@M��@M@M�-@M�@L�/@Lj@K��@K�@KS�@J�@J�!@JM�@I�7@H�9@HbN@H  @G�w@G�P@F��@F��@Fff@F@EV@DZ@D�@C�F@CS�@C33@C@B�!@B^5@A��@@�9@@�u@@r�@@bN@@ �@?�;@?�P@?K�@>�R@=�T@=��@=�@=�@=p�@=p�@=?}@<��@<�@<�j@<�@;��@;33@;o@:��@:~�@:M�@:J@:J@9�@9�#@9�^@9X@9&�@9�@8��@8��@8A�@8  @7�P@7
=@6�R@6v�@6ff@6@5p�@4��@4�@4z�@4(�@3�F@3S�@3"�@3@2�@2�@2��@2~�@2^5@1�@1�7@1X@1&�@0�9@0r�@0Q�@0b@/��@/\)@/
=@.��@.��@.5?@-�@-�h@-p�@-O�@,�@,�@,Z@,(�@+�
@+t�@+S�@+"�@*��@*�\@*~�@*n�@*M�@)��@)x�@)&�@)�@(�`@(Ĝ@(�9@(Q�@'�@'��@'��@'|�@'K�@'
=@&��@&�@&�+@&v�@&5?@&@%@%��@%p�@%?}@$��@$z�@$Z@$I�@$1@#�
@#��@#S�@#@"��@"��@"�!@"^5@"J@!��@!�^@!�7@!hs@!G�@ ��@ �u@ 1'@�@��@��@|�@�y@��@v�@V@$�@�-@O�@��@�D@j@Z@Z@Z@I�@(�@�@��@�F@��@�@dZ@C�@"�@�@��@��@��@n�@^5@=q@-@�@�@��@hs@&�@&�@��@�9@�u@�@Q�@Q�@1'@b@�P@K�@��@��@v�@ff@E�@�@p�@��@��@j@j@(�@��@�m@��@dZ@"�@��@�\@M�@J@��@��@��@��@��@%@�u@A�@1'@  @�@l�@l�@K�@+@
=@ȴ@��@v�@V@V@5?@$�@��@�h@�@`B@O�@/@V@��@�@��@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�  A�A�A�A�A�A�A�A�A�%A�%A�%A�%A���A���Aѧ�Aѡ�Aџ�Aѝ�Aћ�Aћ�Aљ�Aї�Aї�Aї�Aї�AѓuAёhAёhAя\AэPAыDAщ7AхAсA�z�A�p�A�l�A�jA�hsA�ffA�bNA�^5A�ZA�ZA�VA�O�A�E�A�/A���A�%A�z�A�p�A��HA��A�S�A��A��PA��;A��A��#A��A�ffA���A�`BA��A�ȴA��A�5?A�XA��9A�;dA��yA�A�dZA�A��!A���A��PA�(�A��RA��A�$�A��A}��A|�Ay�wAtVAp�jAi�mAcAb  A]�7AX�AVJAT��AR�/AP��AO+AKVAJI�AI`BAF��AE/AC��AA�#A=��A<M�A?�AE�AF��AD�+A@bNA4M�A2=qA0�\A,z�A+�A*�`A*JA*�A)�A)�7A)hsA)��A)7LA(bNA'�A'�A&-A&I�A&M�A&M�A%�mA%l�A%33A$��A$�A#O�A"�A!�TA!K�A ��A (�A $�A��A M�A 1AK�A��AQ�AA��A�7AhsAffAQ�AVA$�A�AdZAVA�HA�+AJAt�AG�AĜA9XA��A��A�A��AC�A�9AZA^5A9XA-A�A��A\)A
=A�A�DA�AA�FAO�A��A1'A1A�A��A�wA�A?}A7LAVA��AA�;Al�A%A�A��AVA$�AJA��A
�9A
�A
A	�A	�-A	hsA	/A�Ar�AbNAVA=qA�A�;A|�AO�A��A�A�DA �A��A`BAA��AjA{A�mA�PA\)A?}AVAĜAZA �A�mA��AhsA/A%AA ��A �A ��A �uA r�A M�A $�A J@���@�\)@�"�@��R@�M�@�5?@�J@��h@��j@�Z@��w@�
=@���@��\@�M�@���@��@���@�\)@��!@�@�O�@��@���@���@��@�;d@���@�-@�^@�X@�bN@�S�@�o@��H@�R@@�~�@�$�@�p�@웦@��@���@�dZ@�;d@�ff@�-@�?}@��@�D@���@��@�@��@���@�D@���@�@���@�\@�^5@��#@�@�&�@�Ĝ@�(�@�dZ@�+@ޟ�@���@ݩ�@��`@�bN@��m@�o@�$�@�O�@ؼj@�Q�@���@�C�@�^5@Չ7@Ԭ@�b@�|�@��@җ�@���@�x�@�hs@�G�@��`@�Z@�33@Χ�@�~�@�5?@�@�hs@�/@���@̛�@�1'@��@˝�@�C�@���@�n�@�{@���@�p�@ț�@�  @Ǯ@�K�@Ɵ�@��@őh@�/@�%@Ĭ@�1'@î@��H@�v�@�=q@��^@�x�@�&�@��/@�bN@�b@��w@�t�@�+@��@��\@�~�@�ff@�V@�$�@��#@�O�@��@��u@�C�@�~�@�J@���@�X@��@�Ĝ@��u@�A�@��@��;@���@��+@�5?@���@�&�@�I�@��;@�|�@�K�@�"�@��@���@��@���@�x�@�`B@�O�@�7L@�I�@��w@��@�;d@��R@�v�@�M�@���@��@���@��9@��@��P@�S�@�33@�
=@��y@���@�n�@��#@�G�@�&�@���@�1'@��
@���@�;d@���@��y@���@���@�^5@�@��#@��-@�x�@�/@��`@���@��u@�I�@��@���@�\)@�"�@���@�v�@�=q@�{@���@���@�p�@�O�@���@�Z@��@��w@�dZ@���@�n�@��-@�`B@��@��@��@���@��P@��@�|�@�l�@�;d@�
=@��@���@���@�/@��@�V@���@��j@���@�r�@�Z@�9X@��m@�dZ@�+@���@���@�^5@��-@�O�@��@���@���@�j@�(�@���@�|�@�33@�o@���@���@�n�@�{@��-@��@���@��D@�j@�Q�@���@�l�@�o@��@��!@�n�@�J@��#@��-@�?}@��u@��@��
@��P@�S�@��@�@��y@���@���@�-@��@�V@��@�r�@�Q�@��@���@��F@��P@�l�@�\)@�C�@�o@���@�E�@�$�@���@��@��^@�hs@��@��j@��u@�z�@�Q�@�b@��
@�t�@�+@��@���@���@��+@�^5@��@��-@�x�@�O�@�V@�%@��`@��j@���@�(�@�w@K�@+@
=@~��@~V@}�T@}��@}O�@}�@|z�@{�m@{�@{C�@{o@z�!@zn�@z^5@zM�@z=q@zJ@y��@xĜ@x�@xQ�@w��@w
=@vv�@vV@u�@t�/@t(�@s�F@s33@r~�@r�@q��@q�^@qG�@p�9@pA�@o;d@o�@o�@n�y@m@m?}@mV@l�D@l9X@k�
@kdZ@j�\@i��@ix�@iX@h�`@h�@hbN@h1'@g�P@f��@e�h@ep�@e?}@d��@d�@d��@d�D@d9X@c��@cƨ@c�F@c�@c33@b�@a�7@aX@a&�@`Ĝ@`�@_�;@_
=@^�@^V@]V@\�@\I�@[�m@[C�@[33@Z�H@Z�\@Y��@X�@W�w@V��@V{@U�h@U?}@T�@T�D@Sƨ@R�@R�H@R��@R^5@R-@Q�#@P�u@P �@O�@O�@O\)@N�y@N�R@N��@N��@N$�@M��@M@M�-@M�@L�/@Lj@K��@K�@KS�@J�@J�!@JM�@I�7@H�9@HbN@H  @G�w@G�P@F��@F��@Fff@F@EV@DZ@D�@C�F@CS�@C33@C@B�!@B^5@A��@@�9@@�u@@r�@@bN@@ �@?�;@?�P@?K�@>�R@=�T@=��@=�@=�@=p�@=p�@=?}@<��@<�@<�j@<�@;��@;33@;o@:��@:~�@:M�@:J@:J@9�@9�#@9�^@9X@9&�@9�@8��@8��@8A�@8  @7�P@7
=@6�R@6v�@6ff@6@5p�@4��@4�@4z�@4(�@3�F@3S�@3"�@3@2�@2�@2��@2~�@2^5@1�@1�7@1X@1&�@0�9@0r�@0Q�@0b@/��@/\)@/
=@.��@.��@.5?@-�@-�h@-p�@-O�@,�@,�@,Z@,(�@+�
@+t�@+S�@+"�@*��@*�\@*~�@*n�@*M�@)��@)x�@)&�@)�@(�`@(Ĝ@(�9@(Q�@'�@'��@'��@'|�@'K�@'
=@&��@&�@&�+@&v�@&5?@&@%@%��@%p�@%?}@$��@$z�@$Z@$I�@$1@#�
@#��@#S�@#@"��@"��@"�!@"^5@"J@!��@!�^@!�7@!hs@!G�@ ��@ �u@ 1'@�@��@��@|�@�y@��@v�@V@$�@�-@O�@��@�D@j@Z@Z@Z@I�@(�@�@��@�F@��@�@dZ@C�@"�@�@��@��@��@n�@^5@=q@-@�@�@��@hs@&�@&�@��@�9@�u@�@Q�@Q�@1'@b@�P@K�@��@��@v�@ff@E�@�@p�@��@��@j@j@(�@��@�m@��@dZ@"�@��@�\@M�@J@��@��@��@��@��@%@�u@A�@1'@  @�@l�@l�@K�@+@
=@ȴ@��@v�@V@V@5?@$�@��@�h@�@`B@O�@/@V@��@�@��@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�mB	M�B
[#B
�)B7LB=qBP�B�B�oB�LB��B��B�B��B��B�oB�PB}�Bl�BN�B<jB(�B�B
=BB
��B
��B
�B
��B
�LB
�B
�hB
`BB
-B
	7B
  B	�B	B	��B	�7B	x�B	q�B	u�B	l�B	`BB	\)B	XB	L�B	I�B	;dB	5?B	49B	+B	/B	49B	%�B	{B	�B	o�B	�B
�B
�B
B	��B	y�B	x�B	W
B	O�B	Q�B	]/B	�B	�JB	�uB	��B	�^B	ÖB	��B	�/B	�mB	�B	��B	��B	��B
B
+B
+B
JB
bB
{B
�B
�B
�B
�B
"�B
%�B
-B
8RB
:^B
8RB
8RB
8RB
6FB
;dB
<jB
B�B
>wB
?}B
@�B
D�B
F�B
G�B
K�B
N�B
O�B
M�B
L�B
L�B
M�B
M�B
L�B
I�B
H�B
H�B
I�B
I�B
H�B
M�B
M�B
N�B
N�B
Q�B
O�B
P�B
VB
T�B
O�B
N�B
VB
VB
R�B
P�B
P�B
O�B
S�B
YB
]/B
XB
ZB
VB
Q�B
VB
[#B
YB
W
B
\)B
[#B
W
B
VB
W
B
XB
Q�B
O�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
O�B
R�B
S�B
VB
Q�B
Q�B
P�B
O�B
N�B
M�B
J�B
H�B
E�B
B�B
A�B
?}B
?}B
A�B
@�B
@�B
?}B
?}B
>wB
=qB
=qB
<jB
;dB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
;dB
;dB
;dB
:^B
:^B
9XB
:^B
8RB
:^B
:^B
;dB
<jB
<jB
:^B
:^B
9XB
7LB
7LB
7LB
6FB
5?B
33B
2-B
33B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
/B
/B
/B
.B
.B
-B
-B
-B
-B
,B
,B
,B
+B
,B
)�B
(�B
'�B
'�B
(�B
'�B
&�B
%�B
%�B
%�B
#�B
#�B
"�B
!�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
hB
hB
hB
hB
hB
oB
hB
oB
bB
\B
\B
bB
hB
bB
bB
bB
bB
bB
\B
\B
\B
VB
VB
PB
JB
JB
DB
JB
JB
JB
JB
DB
JB
DB
DB
DB
DB
DB
DB
DB
DB
JB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
VB
\B
hB
hB
bB
\B
\B
\B
bB
bB
hB
bB
bB
bB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
oB
uB
{B
�B
�B
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
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
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
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
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
G�B
H�B
I�B
I�B
I�B
J�B
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
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
N�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
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
q�B
r�B
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
t�B
t�B
t�B
t�B
t�B
t�B
s�B
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
s�B
t�B
t�B
t�B
u�B
t�B
u�B
t�B
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
{�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�mB	M�B
[#B
�)B7LB=qBP�B�B�oB�LB��B��B�B��B��B�oB�PB}�Bl�BN�B<jB(�B�B
=BB
��B
��B
�B
��B
�LB
�B
�hB
`BB
-B
	7B
  B	�B	B	��B	�7B	x�B	q�B	u�B	l�B	`BB	\)B	XB	L�B	I�B	;dB	5?B	49B	+B	/B	49B	%�B	{B	�B	o�B	�B
�B
�B
B	��B	y�B	x�B	W
B	O�B	Q�B	]/B	�B	�JB	�uB	��B	�^B	ÖB	��B	�/B	�mB	�B	��B	��B	��B
B
+B
+B
JB
bB
{B
�B
�B
�B
�B
"�B
%�B
-B
8RB
:^B
8RB
8RB
8RB
6FB
;dB
<jB
B�B
>wB
?}B
@�B
D�B
F�B
G�B
K�B
N�B
O�B
M�B
L�B
L�B
M�B
M�B
L�B
I�B
H�B
H�B
I�B
I�B
H�B
M�B
M�B
N�B
N�B
Q�B
O�B
P�B
VB
T�B
O�B
N�B
VB
VB
R�B
P�B
P�B
O�B
S�B
YB
]/B
XB
ZB
VB
Q�B
VB
[#B
YB
W
B
\)B
[#B
W
B
VB
W
B
XB
Q�B
O�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
O�B
R�B
S�B
VB
Q�B
Q�B
P�B
O�B
N�B
M�B
J�B
H�B
E�B
B�B
A�B
?}B
?}B
A�B
@�B
@�B
?}B
?}B
>wB
=qB
=qB
<jB
;dB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
;dB
;dB
;dB
:^B
:^B
9XB
:^B
8RB
:^B
:^B
;dB
<jB
<jB
:^B
:^B
9XB
7LB
7LB
7LB
6FB
5?B
33B
2-B
33B
1'B
1'B
1'B
0!B
0!B
0!B
0!B
/B
/B
/B
.B
.B
-B
-B
-B
-B
,B
,B
,B
+B
,B
)�B
(�B
'�B
'�B
(�B
'�B
&�B
%�B
%�B
%�B
#�B
#�B
"�B
!�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
oB
hB
hB
hB
hB
hB
oB
hB
oB
bB
\B
\B
bB
hB
bB
bB
bB
bB
bB
\B
\B
\B
VB
VB
PB
JB
JB
DB
JB
JB
JB
JB
DB
JB
DB
DB
DB
DB
DB
DB
DB
DB
JB
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
JB
VB
\B
hB
hB
bB
\B
\B
\B
bB
bB
hB
bB
bB
bB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
oB
uB
{B
�B
�B
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
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
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
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
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
G�B
H�B
I�B
I�B
I�B
J�B
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
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
N�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
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
q�B
r�B
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
t�B
t�B
t�B
t�B
t�B
t�B
s�B
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
s�B
t�B
t�B
t�B
u�B
t�B
u�B
t�B
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
{�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.40 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200515090047                              AO  ARCAADJP                                                                    20200515090047    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200515090047  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200515090047  QCF$                G�O�G�O�G�O�0               