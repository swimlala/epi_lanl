CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-16T00:35:24Z creation;2017-10-16T00:35:27Z conversion to V3.1;2019-12-19T07:59:10Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20171016003524  20200115121519  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_169                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�-�+�d�1   @�-��Y  @:Ӝ�ߤ�d���E�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D���D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@fff@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBn��BvffB~ffB�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD� DffD�fDffD�fDffD�fDffD��DffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ� D[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDw�fDxffDx�fDyffDy�fDzffDz�fD{ffD{�fD|ffD|�fD}ffD}�fD~ffD~�fDffD�fD�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D�� D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�p D�� D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D�� D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�p D��3D��3D�33D�s3D³3D��3D�33D�s3Dó3D��3D�33D�s3Dĳ3D��3D�33D�s3Dų3D��3D�33D�s3DƳ3D��3D�33D�s3Dǳ3D��3D�33D�s3Dȳ3D��3D�33D�s3Dɳ3D��3D�33D�s3Dʳ3D��3D�33D�s3D˳3D��3D�33D�s3D̳3D��3D�33D�s3Dͳ3D��3D�33D�s3Dγ3D��3D�33D�s3Dϳ3D��3D�33D�s3Dг3D��3D�33D�s3Dѳ3D��3D�33D�s3Dҳ3D��3D�33D�s3Dӳ3D��3D�33D�s3DԳ3D��3D�33D�s3Dճ3D��3D�33D�s3Dֳ3D��3D�6fD�vfD׳3D��3D�33D�s3Dس3D��3D�33D�s3Dٳ3D��3D�33D�s3Dڳ3D�� D�33D�s3D۳3D��3D�33D�s3Dܳ3D��3D�33D�s3Dݳ3D��3D�33D�s3D޳3D��3D�33D�s3D߳3D��3D�33D�s3D�3D��3D�33D�s3D� D��3D�33D�s3D�3D��3D�0 D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D��3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D�3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��3D�33D�s3D��3D��fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՍPAՋDAՍPAՍPAՍPAՏ\AՓuAՓuAՑhAՕ�AՓuAՏ\AՍPAՍPAՓuAՓuA՗�Aՙ�A՗�Aՙ�Aՙ�AՕ�AՑhAՕ�A՝�A՛�AՇ+A�r�AՋDA�~�A�^5A�-A��AΑhA�t�A�ƨA���A�jA�"�A��jA��wA�9XA�E�A�K�A��A���A��mA�VA�ƨA��PA�ZA� �A�t�A��DA���A��A�5?A�E�A�$�A��;A�$�A�$�A��A�O�A�p�A��/A�O�A���A�ffA�1'A�5?A�jA��A�"�A�Q�A��hA�ĜA�A���A�A�A�
=A���A�ȴA�^5A��A�;dA�(�A��A��A�Q�A��
A�bA�bNA��#A��RA��\A�/A~v�A}�A}��A|�+A{�Az�RAy��Ay�PAy�Ax1AwO�Av�yAvz�Au��At�!AtffAt1'As�wArQ�Aq|�Aq%Ap1AnM�AmC�Al �Ak&�Ah��Ag��Ag7LAf�uAep�AdZAc�Ac`BAc33Ab�!Ab^5Aa��Aa7LA_�mA_�A^��A^ �A]C�A\=qA[��A[VAZn�AZ�AY|�AYK�AW��AUt�AT~�ATA�AS%AQt�APr�AOXANr�AM��AM�AMl�AM�AK&�AI�AH�AG��AG�AGK�AG�AF��AF=qAE
=ACXAB=qAA�AA�TAA��A?p�A=?}A<��A;�TA:�uA9��A9p�A8�yA8n�A7�A7`BA6jA4ȴA4n�A3�
A2VA1��A1�-A1%A0�DA0E�A/A-��A+dZA*M�A)��A)�FA)�A)|�A(��A'\)A&��A& �A&1A%�-A$�jA$1A#t�A"�`A!��A!�A ��A �DA bNA  �AK�A7LA~�AI�A �A��AO�A�A1'A��A�7A�7A|�A|�A\)A7LA�!AA�A�A
=AJA�FAt�A�+AO�A��A��A1At�A-A"�AVA�A+AƨA�A
��A
bA�A^5A��A33AjA��A1'AG�A�A^5A ��@�S�@�@��@���@�V@�+@�hs@�I�@���@�V@��T@� �@�33@�@�j@�V@��@◍@�p�@���@��@�G�@ڸR@�7L@��@��#@���@Ӆ@�n�@��@�&�@�r�@�1@� �@�C�@�~�@̛�@���@���@�O�@�I�@��@��@�5?@��#@�%@Ĭ@�o@�@�v�@���@��u@��@�v�@��^@�?}@���@�o@��@�V@�9X@�ƨ@��y@��#@�p�@���@�Q�@���@��y@�hs@�%@���@�j@�A�@���@�$�@��^@��@�Ĝ@�j@�b@�C�@��\@���@��9@�b@��w@�K�@���@�{@��-@�X@��@��@�33@�x�@��y@��/@��@�b@�o@�@��h@��@���@���@���@�;d@���@��R@�ȴ@��\@�J@��@�J@���@�hs@��/@�9X@�33@�-@�hs@��@��`@��j@��9@��@�z�@�I�@��@��;@�ƨ@�l�@��@�v�@�J@��-@�&�@��j@�j@�1@�dZ@��R@�{@��@�hs@�hs@�X@�Ĝ@��u@�bN@� �@�1@��@�ƨ@�S�@�~�@�E�@�@��T@��#@���@���@�V@�Ĝ@�Ĝ@��D@�j@�9X@���@�dZ@�C�@�o@��@���@��!@���@�n�@��#@���@�x�@�`B@�X@�O�@�7L@��@���@���@���@��D@��@��@�A�@K�@~5?@}�T@}@}��@}`B@|�/@|I�@{ƨ@{��@{dZ@{"�@z�H@y�^@yx�@y7L@x�`@x��@xA�@w��@w�@w�P@w�P@w\)@v�+@u��@t�/@s�m@s�
@sƨ@sƨ@sƨ@s�
@st�@s��@sC�@q��@p��@o�@o\)@o;d@o�@n�@nȴ@n�+@n@m�@m?}@m/@l��@lj@k�m@kt�@k"�@j~�@h��@hbN@g�@g��@g��@g�w@g��@gl�@f�y@f�R@f{@dj@c�F@c�@cdZ@c33@b�@b�!@a�@a7L@`�9@`A�@`b@_��@_��@^�y@^�@^ȴ@^��@^ff@^5?@^$�@^$�@^@]V@\��@\I�@[��@[�
@[t�@[@Z=q@Yhs@XQ�@WK�@Vȴ@U�@UV@T�@T��@TZ@S��@S�
@Sƨ@St�@R�H@Rn�@Q��@Q�#@Q��@Q�^@Q��@Q�7@Qx�@Qhs@Q%@P�9@P  @O+@N�R@N��@N�+@NE�@N@M��@Mp�@M`B@M?}@M�@M�@Mp�@M��@M�@LZ@L(�@L(�@Lj@LZ@LZ@LZ@LZ@Lj@L�j@L9X@L(�@K��@Kƨ@K�@K"�@J�!@J~�@J~�@J^5@J�@J-@J=q@J=q@I�@I��@IG�@I7L@H��@H��@H�@H�@Hr�@HbN@H�@HbN@HbN@H�@HbN@HQ�@H  @G�P@F�y@FE�@F{@E�@E�T@E@Ep�@E�@D�D@D1@C�
@C�F@C��@Ct�@CdZ@CC�@C33@Co@B��@B�@A��@@��@?�@?��@?��@>�+@=�@=p�@=/@<�@<��@<�@<I�@<1@;ƨ@;dZ@;33@;o@:�H@:�\@:M�@:-@9��@81'@7��@7�@6�@6��@6�+@6ff@5��@5`B@5?}@5/@5�@5V@4�j@4j@49X@3ƨ@3S�@333@3o@2�H@2n�@2M�@2�@1��@1x�@1X@1G�@1%@0��@0�9@0�@0Q�@0b@/��@/�w@/�P@/K�@.��@-p�@,�@,��@,�D@,z�@,Z@,9X@,(�@+�m@+dZ@*�@*�H@*~�@*=q@)��@)7L@(��@(Q�@'��@'��@';d@&��@&��@&��@&��@&�+@&E�@%�@%��@%V@$�@$��@$�j@$j@#�m@#ƨ@#ƨ@#��@#33@#@"�@"��@"M�@!��@!��@!x�@ �u@ bN@ bN@ bN@ bN@ r�@ r�@ r�@ r�@ r�@ r�@ Q�@��@��@l�@�y@ȴ@ff@�-@��@�h@�@`B@O�@�@�/@��@I�@1@ƨ@�F@��@dZ@C�@@�H@�!@��@�\@�\@�\@~�@=q@�@�@��@��@��@hs@7L@&�@�@%@%@��@�9@r�@A�@  @�@�@�;@�;@��@�P@�@�y@�@�R@v�@E�@5?@@�T@��@�-@�@V@�j@�D@j@(�@ƨ@��@t�@S�@@�H@�H@�H@�H@��@��@��@�!@��@^5@=q@-@�@�#@hs@hs@X@G�@7L@7L@��@�`@��@r�@��@��@|�@\)@\)@\)@K�@\)@\)@K�@�@��@�@�@ȴ@�R@v�@V@�@@��@�h@�@�@p�@O�@�@�j@�@Z@�m@��@t�@C�@"�@@
��@
�!@
�\@
^5@
M�@
=q@
�@	��@	�@	�^@	x�@	7L@	�@	%@��@�9@��@r�@Q�@A�@1'@1'@ �@�@�;@��@�@��@�P@l�@\)@K�@�@��@�y@�@�R@��@�+@$�@�T@�-@�h@�h@�@p�@?}@�@��@Z@9X@�@��@dZ@dZ@S�@C�@@�!@~�@n�@=q@J@�@��@hs@&�@ �`@ �`@ ��@ ��@ �@ A�@  �@   ?��;?��;?��w?�|�?���?��R?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՍPAՋDAՍPAՍPAՍPAՏ\AՓuAՓuAՑhAՕ�AՓuAՏ\AՍPAՍPAՓuAՓuA՗�Aՙ�A՗�Aՙ�Aՙ�AՕ�AՑhAՕ�A՝�A՛�AՇ+A�r�AՋDA�~�A�^5A�-A��AΑhA�t�A�ƨA���A�jA�"�A��jA��wA�9XA�E�A�K�A��A���A��mA�VA�ƨA��PA�ZA� �A�t�A��DA���A��A�5?A�E�A�$�A��;A�$�A�$�A��A�O�A�p�A��/A�O�A���A�ffA�1'A�5?A�jA��A�"�A�Q�A��hA�ĜA�A���A�A�A�
=A���A�ȴA�^5A��A�;dA�(�A��A��A�Q�A��
A�bA�bNA��#A��RA��\A�/A~v�A}�A}��A|�+A{�Az�RAy��Ay�PAy�Ax1AwO�Av�yAvz�Au��At�!AtffAt1'As�wArQ�Aq|�Aq%Ap1AnM�AmC�Al �Ak&�Ah��Ag��Ag7LAf�uAep�AdZAc�Ac`BAc33Ab�!Ab^5Aa��Aa7LA_�mA_�A^��A^ �A]C�A\=qA[��A[VAZn�AZ�AY|�AYK�AW��AUt�AT~�ATA�AS%AQt�APr�AOXANr�AM��AM�AMl�AM�AK&�AI�AH�AG��AG�AGK�AG�AF��AF=qAE
=ACXAB=qAA�AA�TAA��A?p�A=?}A<��A;�TA:�uA9��A9p�A8�yA8n�A7�A7`BA6jA4ȴA4n�A3�
A2VA1��A1�-A1%A0�DA0E�A/A-��A+dZA*M�A)��A)�FA)�A)|�A(��A'\)A&��A& �A&1A%�-A$�jA$1A#t�A"�`A!��A!�A ��A �DA bNA  �AK�A7LA~�AI�A �A��AO�A�A1'A��A�7A�7A|�A|�A\)A7LA�!AA�A�A
=AJA�FAt�A�+AO�A��A��A1At�A-A"�AVA�A+AƨA�A
��A
bA�A^5A��A33AjA��A1'AG�A�A^5A ��@�S�@�@��@���@�V@�+@�hs@�I�@���@�V@��T@� �@�33@�@�j@�V@��@◍@�p�@���@��@�G�@ڸR@�7L@��@��#@���@Ӆ@�n�@��@�&�@�r�@�1@� �@�C�@�~�@̛�@���@���@�O�@�I�@��@��@�5?@��#@�%@Ĭ@�o@�@�v�@���@��u@��@�v�@��^@�?}@���@�o@��@�V@�9X@�ƨ@��y@��#@�p�@���@�Q�@���@��y@�hs@�%@���@�j@�A�@���@�$�@��^@��@�Ĝ@�j@�b@�C�@��\@���@��9@�b@��w@�K�@���@�{@��-@�X@��@��@�33@�x�@��y@��/@��@�b@�o@�@��h@��@���@���@���@�;d@���@��R@�ȴ@��\@�J@��@�J@���@�hs@��/@�9X@�33@�-@�hs@��@��`@��j@��9@��@�z�@�I�@��@��;@�ƨ@�l�@��@�v�@�J@��-@�&�@��j@�j@�1@�dZ@��R@�{@��@�hs@�hs@�X@�Ĝ@��u@�bN@� �@�1@��@�ƨ@�S�@�~�@�E�@�@��T@��#@���@���@�V@�Ĝ@�Ĝ@��D@�j@�9X@���@�dZ@�C�@�o@��@���@��!@���@�n�@��#@���@�x�@�`B@�X@�O�@�7L@��@���@���@���@��D@��@��@�A�@K�@~5?@}�T@}@}��@}`B@|�/@|I�@{ƨ@{��@{dZ@{"�@z�H@y�^@yx�@y7L@x�`@x��@xA�@w��@w�@w�P@w�P@w\)@v�+@u��@t�/@s�m@s�
@sƨ@sƨ@sƨ@s�
@st�@s��@sC�@q��@p��@o�@o\)@o;d@o�@n�@nȴ@n�+@n@m�@m?}@m/@l��@lj@k�m@kt�@k"�@j~�@h��@hbN@g�@g��@g��@g�w@g��@gl�@f�y@f�R@f{@dj@c�F@c�@cdZ@c33@b�@b�!@a�@a7L@`�9@`A�@`b@_��@_��@^�y@^�@^ȴ@^��@^ff@^5?@^$�@^$�@^@]V@\��@\I�@[��@[�
@[t�@[@Z=q@Yhs@XQ�@WK�@Vȴ@U�@UV@T�@T��@TZ@S��@S�
@Sƨ@St�@R�H@Rn�@Q��@Q�#@Q��@Q�^@Q��@Q�7@Qx�@Qhs@Q%@P�9@P  @O+@N�R@N��@N�+@NE�@N@M��@Mp�@M`B@M?}@M�@M�@Mp�@M��@M�@LZ@L(�@L(�@Lj@LZ@LZ@LZ@LZ@Lj@L�j@L9X@L(�@K��@Kƨ@K�@K"�@J�!@J~�@J~�@J^5@J�@J-@J=q@J=q@I�@I��@IG�@I7L@H��@H��@H�@H�@Hr�@HbN@H�@HbN@HbN@H�@HbN@HQ�@H  @G�P@F�y@FE�@F{@E�@E�T@E@Ep�@E�@D�D@D1@C�
@C�F@C��@Ct�@CdZ@CC�@C33@Co@B��@B�@A��@@��@?�@?��@?��@>�+@=�@=p�@=/@<�@<��@<�@<I�@<1@;ƨ@;dZ@;33@;o@:�H@:�\@:M�@:-@9��@81'@7��@7�@6�@6��@6�+@6ff@5��@5`B@5?}@5/@5�@5V@4�j@4j@49X@3ƨ@3S�@333@3o@2�H@2n�@2M�@2�@1��@1x�@1X@1G�@1%@0��@0�9@0�@0Q�@0b@/��@/�w@/�P@/K�@.��@-p�@,�@,��@,�D@,z�@,Z@,9X@,(�@+�m@+dZ@*�@*�H@*~�@*=q@)��@)7L@(��@(Q�@'��@'��@';d@&��@&��@&��@&��@&�+@&E�@%�@%��@%V@$�@$��@$�j@$j@#�m@#ƨ@#ƨ@#��@#33@#@"�@"��@"M�@!��@!��@!x�@ �u@ bN@ bN@ bN@ bN@ r�@ r�@ r�@ r�@ r�@ r�@ Q�@��@��@l�@�y@ȴ@ff@�-@��@�h@�@`B@O�@�@�/@��@I�@1@ƨ@�F@��@dZ@C�@@�H@�!@��@�\@�\@�\@~�@=q@�@�@��@��@��@hs@7L@&�@�@%@%@��@�9@r�@A�@  @�@�@�;@�;@��@�P@�@�y@�@�R@v�@E�@5?@@�T@��@�-@�@V@�j@�D@j@(�@ƨ@��@t�@S�@@�H@�H@�H@�H@��@��@��@�!@��@^5@=q@-@�@�#@hs@hs@X@G�@7L@7L@��@�`@��@r�@��@��@|�@\)@\)@\)@K�@\)@\)@K�@�@��@�@�@ȴ@�R@v�@V@�@@��@�h@�@�@p�@O�@�@�j@�@Z@�m@��@t�@C�@"�@@
��@
�!@
�\@
^5@
M�@
=q@
�@	��@	�@	�^@	x�@	7L@	�@	%@��@�9@��@r�@Q�@A�@1'@1'@ �@�@�;@��@�@��@�P@l�@\)@K�@�@��@�y@�@�R@��@�+@$�@�T@�-@�h@�h@�@p�@?}@�@��@Z@9X@�@��@dZ@dZ@S�@C�@@�!@~�@n�@=q@J@�@��@hs@&�@ �`@ �`@ ��@ ��@ �@ A�@  �@   ?��;?��;?��w?�|�?���?��R?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��BcTBffBBÖBȴBB�jB�LB��B�VB�By�Bm�BE�B#�BhBoB�B�B\BBVB�B��BƨB��B��B�3B��B��B�B�7B�PB�DB�B� Bv�Bo�Be`BR�BI�BA�B2-B'�B�B�BoB\BJBB  B
��B
�B
�TB
�;B
��B
��B
�LB
�B
��B
��B
��B
��B
��B
�bB
�JB
�7B
�B
{�B
r�B
m�B
iyB
dZB
\)B
XB
T�B
Q�B
M�B
I�B
I�B
G�B
C�B
;dB
8RB
6FB
/B
#�B
�B
�B
PB
B	��B	��B	��B	�B	�yB	�mB	�`B	�TB	�BB	�/B	�B	��B	��B	ɺB	ƨB	��B	�jB	�FB	�-B	�B	�B	��B	��B	��B	��B	�7B	�%B	�B	}�B	w�B	w�B	r�B	p�B	o�B	n�B	m�B	jB	aHB	ZB	S�B	O�B	N�B	M�B	L�B	J�B	F�B	?}B	6FB	2-B	1'B	0!B	,B	 �B	uB	{B	bB		7B	+B	%B	B	  B��B��B�B�B�B�mB�BB�BB�BB�)B�B�
B��BȴB�}B��BĜBĜBB��B�jB�9B�FB�3B�?B�-B�B��B��B��B��B��B��B��B��B��B�{B�7B�JB�VB�VB�JB�7B�+B�+B�B�%B�+B�%B�%B�B�B~�B|�By�Bu�Br�Bs�Br�Bk�BgmBk�Bl�BffBe`B^5B\)B[#BYBVBQ�BR�BT�BP�BM�BQ�BQ�BQ�BL�BJ�BC�BA�BA�B?}B8RB6FB8RB7LB:^B6FB/B33B5?B2-B0!B,B/B1'B0!B-B)�B-B(�B'�B+B%�B"�B�B"�B$�B �B�B �B �B!�B!�B'�B+B-B-B/B,B(�B.B0!B/B1'B0!B49B7LB5?B9XB9XB@�BB�B@�B;dB:^B;dB?}BD�BE�BC�BF�BJ�BJ�BL�BJ�BL�BO�BP�BP�BQ�BR�BQ�BXBYBZBZBW
B\)B_;B`BBbNBcTBcTBbNBdZBhsBffBk�Bn�Bo�Bq�Br�Bw�Bx�Bw�Bu�Bt�Bs�Bs�Bx�B�B�B~�B�B�B�1B�PB�PB�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�-B�-B�FB�FB�LB�RB�qB�}B��BÖBǮBɺB��B��B��B�
B�#B�BB�NB�`B�`B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	+B		7B		7B	1B	JB	VB	\B	hB	oB	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	!�B	!�B	!�B	"�B	$�B	&�B	'�B	'�B	&�B	(�B	/B	33B	49B	49B	49B	5?B	7LB	:^B	>wB	?}B	@�B	B�B	A�B	E�B	F�B	F�B	F�B	G�B	J�B	M�B	M�B	O�B	Q�B	Q�B	VB	YB	[#B	`BB	aHB	aHB	aHB	aHB	`BB	`BB	_;B	]/B	aHB	e`B	k�B	m�B	m�B	m�B	n�B	n�B	n�B	p�B	r�B	s�B	s�B	t�B	t�B	u�B	v�B	v�B	x�B	~�B	�B	�B	�+B	�1B	�1B	�7B	�7B	�DB	�=B	�=B	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�!B	�'B	�3B	�3B	�9B	�9B	�FB	�LB	�^B	�wB	�}B	B	ƨB	ǮB	ƨB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
+B
+B
+B
1B
1B
	7B
DB
JB
JB
JB
JB
PB
VB
\B
hB
hB
hB
hB
hB
hB
oB
hB
hB
hB
oB
hB
uB
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
#�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
/B
.B
.B
1'B
2-B
33B
33B
33B
33B
33B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
6FB
6FB
6FB
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
8RB
8RB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
>wB
@�B
A�B
A�B
B�B
A�B
@�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
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
K�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
Q�B
R�B
S�B
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
YB
XB
ZB
ZB
ZB
ZB
ZB
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
ZB
[#B
[#B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
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
aHB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�)B�)B�)B�CB�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�CB�)B�)B�)B�)B�CB�)B�)B�/B�/B�CB�WB�=B�B�qB��B��B�5BqvBv�B�_B��B˒BŢB��B�xB�KB��B�fB}qBq�BM�B*B�B�BdB�B�BtB4B�B�B�lBϫB��B��B��B�kB�B�DB��B��B�gB��By�Br-Bg�BV�BKxBC�B4B)�BB�BuBHBjBSB;B
�BB
�B
��B
�B
ּB
��B
��B
�B
��B
�B
�BB
�jB
�	B
�oB
�B
�	B
�{B
}B
tB
n�B
jKB
eFB
]�B
YB
U�B
R�B
N�B
J�B
JXB
H1B
D�B
="B
9rB
72B
0�B
%�B
5B
B
(B
�B	�HB	��B	��B	�;B	��B	�>B	��B	��B	�B	��B	�	B	�2B	�PB	ʌB	ǔB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�0B	�zB	��B	�B	y�B	yXB	t9B	q�B	poB	oB	nB	k�B	c�B	[�B	U�B	P�B	OvB	NVB	MPB	KxB	HB	AoB	8lB	3�B	1�B	0�B	-)B	#�B	B	�B	�B	
�B	KB	�B	�B	 �B��B��B�TB�B�]B�B�B�B��B�IB�B�B��B��B�AB��B�9B�B�-B�B��B�B�2B�9B��B��B��B�6B�B�B�NB��B�vB�\B�5B��B�B��B�PB��B��B�B�=B�KB�B�B�tB�zB��B�tB��B��B� B}�Bz�Bv�BtBt�Bs�BmBh�Bl=Bm)Bg�Bf�B`'B]�B\xBZBW�BS�BT,BU�BR BOvBSBS&BR�BN<BL0BE�BB�BB�B@�B:xB8B9�B8�B;0B7�B1[B4�B6`B3�B1�B.IB0oB2-B1AB.cB+�B.cB*�B)B+�B'�B$ZB!�B$B&B"�B!B!bB!�B"�B"�B(�B+�B-wB-�B0!B-wB*eB/ B0�B0B1�B1'B4�B7�B6+B:B:�BABB�BAUB<�B;JB<jB@OBESBFYBEBG�BK�BK�BM�BK�BM�BP}BQ�BQ�BR�BS�BSBX�BY�BZ�BZ�BX+B\�B_�B`�Bb�Bc�Bc�Bc:Be,BiDBg�Bl=BoBp;BraBshBxRByrBx�Bv�Bu�Bu?Bu�BzDB��B��B�B��B��B��B��B�"B��B��B��B��B��B�B�1B�B�,B�sB��B��B��B��B��B��B��B�qB�wB�IB�UB�|B��B��B��B��B��B�	B��B�B�B�3B�1B�=B�^B�~BѝB��B��B��B�B��B��B��B�B�B�B�B�3B�MB�B�2B�8B�*B�6B�PB�jB��B	oB	mB	�B		�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	B	+B	B	B	!B	"B	"4B	"B	"4B	"4B	#:B	%FB	'8B	($B	(>B	'mB	)�B	/�B	3hB	4�B	4nB	4�B	5�B	7�B	:�B	>�B	?�B	AB	B�B	B'B	E�B	GB	GB	GB	HB	K)B	N"B	N"B	P.B	RTB	R�B	V�B	Y�B	[�B	`vB	a�B	a|B	a|B	a|B	`�B	`�B	_�B	^B	a�B	e�B	k�B	m�B	m�B	m�B	n�B	o B	o B	qB	r�B	tB	tB	u%B	u%B	v+B	w2B	wfB	y�B	cB	�[B	�mB	�EB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�/B	�'B	�4B	�,B	�2B	�2B	�LB	�B	�DB	�DB	�KB	�QB	�=B	�WB	�kB	��B	�wB	�oB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	��B	��B	�+B	�B	��B	�B	�#B	�)B	�6B	�(B	�.B	�B	�4B	�:B	�:B	�@B	�@B	�TB	�TB	�uB	�{B	�yB	�kB	�kB	�QB	�qB	�xB	ބB	ߊB	��B	��B	�|B	�hB	�B	�B	��B	�B	��B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�	B	�B	��B	�^B	�B	�6B	�]B	�cB	�HB
 OB
AB
GB
GB
MB
mB
SB
_B
zB
zB
�B
�B
�B
	�B
�B
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
+B
�B
B
�B
�B
�B
�B
B
�B
B
B
B
B
B
B
B
B
!B
dB
B
!B
�B
"B
"B
"B
"NB
$&B
&2B
&B
&2B
&2B
&LB
&2B
'8B
(XB
(XB
*KB
*KB
*KB
*eB
+QB
+QB
,WB
,WB
-]B
.cB
.IB
.cB
.cB
/iB
/iB
/iB
/OB
0oB
0oB
/iB
.�B
.�B
1�B
2|B
3hB
3�B
3�B
3�B
3�B
2|B
2�B
3�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
5�B
5�B
5�B
6zB
6�B
6�B
5�B
5tB
5�B
5�B
6�B
6zB
6�B
6�B
6�B
8�B
8�B
7�B
7�B
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
=�B
>�B
@�B
A�B
A�B
B�B
A�B
@�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
EB
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
IB
IB
IB
IB
IB
J	B
I�B
J	B
J�B
J�B
J	B
J	B
J#B
KB
KB
K�B
K�B
K�B
K�B
LB
L0B
L0B
MB
MB
MB
N"B
N"B
OB
N<B
O(B
OBB
O(B
O(B
OBB
P.B
P.B
Q4B
Q4B
Q4B
R:B
R:B
S@B
R:B
S@B
TB
T,B
T,B
T,B
TB
T,B
T,B
TaB
TFB
UMB
U2B
UMB
UMB
VSB
W?B
W?B
X+B
XEB
XEB
X_B
YKB
YeB
YB
XyB
ZkB
ZkB
ZQB
ZQB
ZQB
ZQB
ZQB
Z7B
ZkB
ZkB
ZkB
ZkB
[WB
[WB
[qB
ZQB
[qB
[qB
\xB
\xB
]dB
]dB
]IB
\]B
\xB
\xB
]~B
]~B
]~B
]�B
^�B
_�B
_pB
`�B
`�B
`�B
`�B
a�B
a�B
a|B
a|B
a�B
a�B
b�B
a�B
a�B
b�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e�B
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
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�1111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<`w<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.4(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710200033332017102000333320171020003333201806221232112018062212321120180622123211201804050427442018040504274420180405042744  JA  ARFMdecpA19c                                                                20171016093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171016003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171016003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171016003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171016003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171016003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171016003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171016003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171016003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171016003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20171016005651                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171016153313  CV  JULD            G�O�G�O�F�o�                JM  ARGQJMQC2.0                                                                 20171016153313  CV  JULD_LOCATION   G�O�G�O�F�o�                JM  ARGQJMQC2.0                                                                 20171016153313  CV  LONGITUDE       G�O�G�O��$d                JM  ARSQJMQC2.0                                                                 20171017000000  CF  PSAL_ADJUSTED_QCB���D�� G�O�                JM  ARCAJMQC2.0                                                                 20171019153333  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171019153333  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192744  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033211  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121519                      G�O�G�O�G�O�                