CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:51:09Z creation;2022-06-04T17:51:09Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175109  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�x=��1   @�y5��@0�r� Ĝ�c5XbM�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@y��@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�ffB�  B�  B�  B�  Bߙ�B���B�ffB�  B�  B�  B�  B�  C   C  C�C� C  C	�3C  C�C�fC�fC  C  C  C  C  C  C   C"ffC#��C%�fC(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CT  CV  CX  CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"�fD#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ DӼ�D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@w
>@��R@��RAA?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B߅B�RB�Q�B��B��B��B��B��B��C��C]Cu�C��C	��C��C]C�)C�)C��C��C��C��C��C��C��C"\)C#C%�)C'��C)�)C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CP]CR]CS��CU��CW��CY��C[��C]��C_�)Ca�)Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"��D"�qD#}qD#�qD$}qD$�qD%}qD%�D&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�DI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�D^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDx�Dx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�{�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�Dӻ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D���D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�8R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��2A���A��XA��A���A��gA���A��A��ZA���A��A��A�+�A�9�A�G�A�i�A˰UA�՛A���A���A�
rA��A�SA��A�.A��A��A�A�	lA�A��VA��A��`A��+A��vA���A��,Aʀ4A��A��gA�hsA�@�A� �A� �A�͟AǱ�A�H�A��A��?AŨ$AĀ A���A��A��
A�;�A�p�A���A�&A���A�  A�"A���A���A��	A�$@A�OvA�>�A��A��HA��RA�zDA�)�A���A�&�A��A��hA�M�A��2A��]A�W
A���A~)�As��Aq�An~�Ak�Aec A_4A\�hAXU�ATیAR5?AO�HAN;�AK� AH1�AF�tAEb�ABMA@e�A?��A?/A>�A<e�A;$�A; iA:Y�A9�AA9XyA9�A8��A8A�A6<�A0)�A/�DA/V�A/�eA0��A/��A/t�A.-wA-�cA-8�A,c A*6zA)A(;�A&hsA%4�A$6A#XA"�A"e,A!��A!XA!�eA":�A"X�A"^�A!�vA!�A e�A $tA��A[WA�A��A4nA�A��A4A�-A��A�{A�A�xA;Au�A��Ai�AOA �A|�A�CA
�A��A�AA;�AM�AHA!Av�A�oAW�A��AϫA�A�A;Am]A+A
�'A
`BA
�A	y>A	xA�SAy>AC�A҉A6zA��A�hAS&AqA�A��AXA�A�hA�A�A3�A�'A�}AO�A	�A �pA ��A Z�@�
=@�`�@��@��@�'�@��.@��o@�ff@��3@�|�@���@���@�1'@�Mj@���@�*�@��@��/@��@�Q@��@��@�ߤ@���@��N@�+@�]d@�e,@��/@셈@�7�@�7�@���@���@�@��@�!@��@�4@�M�@�tT@�S&@���@��@�z@�j@��@�x@���@��@�S&@���@�kQ@��@��@��;@�M@�8�@��@�r@�!@��.@��@�}�@�L�@�/�@�!-@��@�ߤ@�^5@�.�@���@�@���@߮�@�L0@��>@ݞ�@�zx@��@��T@۫�@ڶ�@ٝ�@ءb@���@�x�@��+@��H@� �@�;d@��@��'@ґ�@��d@� i@���@Ы6@���@Ϥ@@���@�P�@���@��[@̣@̜x@̕@̗�@̏\@�.�@˗$@�Y�@�!-@��@��8@ʯO@�Ft@��@�s�@��@ȼj@ȸR@ȴ9@ȱ�@ȸR@�u%@ǩ*@�Z�@�Mj@�?}@���@ƞ@�5?@���@Ŭq@Ŕ�@�a@�0�@�l�@�_p@��@��,@�t�@��@��w@��:@�:�@�Ĝ@���@�@�/�@��f@�m�@��@��@�u�@�{�@�	�@�o�@�!�@��@���@��P@��8@���@��+@��q@�J#@�6z@�+@��@��u@�~�@�`�@�GE@�oi@��o@��@�*0@��@�?�@�N�@�'R@��#@�@O@�%@��@���@�M�@�M@���@���@���@�qv@�RT@��@�I�@��@��;@���@�!�@��K@��)@���@�[�@�
�@��F@��~@�O@��@�@���@�O@���@�g�@�@O@��@�V@��@���@���@� �@��@���@�A�@��2@���@�:*@���@���@�n/@�k�@�]�@�+@�{�@���@�A @�Y@��@�V@��@��@�e�@��m@���@�K�@�/@��@���@���@��@�w�@�I�@�"h@��@��@��}@��@@�a�@�C@���@��+@��@�1@��@�w2@�+�@��@���@�)�@���@���@���@�k�@�S�@�8@���@���@�l�@�O@��@���@�@O@�ߤ@��h@��_@���@�h
@�	@���@��F@���@�n/@� i@���@��b@�c @���@��t@���@�rG@�)_@��!@�c @�,=@��K@�w2@��@���@�N�@�.�@�-�@�/�@��@��@��@�c�@��@��6@�J�@��@��z@���@��"@�qv@� \@���@�q@�'R@���@��z@���@���@�Vm@�?}@�@@��@��@�~(@�B[@�_@���@�>�@���@�ȴ@�� @�_@�'R@��a@�y�@��2@���@�tT@�.�@��g@�ƨ@���@�|�@�>�@��P@�Ɇ@��@�w�@�K^@�$�@��@���@���@���@�b�@���@��@���@�h
@��j@���@���@��S@���@��:@��4@�b�@�8�@��2@��h@���@�c @��@��@�V@��@��@\)@~ߤ@~�+@~{@}�=@}q@|�@|��@|�@|%�@{��@{�@z�8@z�@zں@z��@z�m@z�@z��@zL0@z�@y�@y��@y�@xA�@w��@w�$@wb�@w'�@v{�@u��@u��@uT�@u�@t��@tbN@s�@sb�@r�2@ra|@q�@p��@pl"@o�k@n�2@n�@m�7@mq@loi@k�@kqv@k@O@j�@je@i%@h�@gs@go@f�@f�]@f:*@ec@d�	@d��@dI�@d�@c�}@b�]@b
�@a�9@a\�@`I�@_�@_�}@_��@^z@^5?@]�>@]|@\�`@\�D@\H@[�r@[خ@[�w@[�k@[\)@[)_@Z�!@Z��@ZGE@Y|@Y%@X�@Wx@U�@U��@U}�@U=�@T�|@T��@T�o@TZ@T*�@T@S�@S�F@S�k@S��@Sj�@S\)@S�@Rv�@Q�~@QG�@P��@P�@O�[@Ox@Oe�@O1�@N��@N)�@M��@M�@L��@L��@Ll"@L,=@K�q@K�	@KK�@K+@J��@J��@J�A@J1�@I�@Im]@Iq@Hy>@G��@G�@G��@GX�@Fں@FJ@E��@E��@E�@E��@E�-@E�n@DĜ@C��@C��@C��@C9�@C�@B��@BE�@A�H@Au�@A;@@�.@@S�@?��@?s@?W?@?�@>�x@>B[@=��@=�h@=IR@=q@=%@<��@<,=@<x@;��@;�@;x@;y�@;iD@;@O@:ں@:~�@:&�@9�@9�'@9�S@9�"@9`B@9/@8�P@8�E@8�9@8�@8N�@8x@7خ@7��@7A�@6�h@6@�@6&�@64@5�@5�N@5��@5L�@4Ɇ@4��@4�Y@4,=@3˒@3J#@3!-@3�@2�@2�'@2�@2��@2a|@1�j@1m]@1q@0�U@0�@0��@0u�@06@02�@/˒@/�@.��@.?@.?@.1�@.e@-�.@-��@-c@-hs@-[W@-O�@-F@-?}@-5�@-%@,>B@+A�@*�"@*��@*�'@*�@*}V@*R�@*�@)��@)�t@)��@)��@)��@)��@)��@)e,@)&�@(ی@(6@'F�@'$t@&��@&�}@&~�@&R�@&6�@&�@%��@%��@%��@%�n@%�h@%��@%[W@%@@$�f@$�@$z�@#�+@#خ@#��@#��@#b�@#e�@#t�@#x@#v`@#qv@#A�@#S@"�m@"J�@!��@!s�@!?}@!#�@!�@!@ �@ ��@ �@ `�@ �@�@�;@��@y�@b�@H�@
=@��@��@��@p;@3�@_@�#@Q�@�K@�[@ѷ@�@��@�o@e�@7�@  @�	@W?@>�@1�@�@�]@�+@0U@�@��@��@T�@	l@��@*�@�@�}@��@C@�@҉@ȴ@��@��@�+@��@u%@_�@J�@6�@ �@�9@�@��@�h@e,@0�@(�@ \@�@֡@|�@(�@  @خ@�*@��@��@x@dZ@Mj@��@�b@�\@d�@H�@5?@{@��@�N@c@=�@%F@�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��2A���A��XA��A���A��gA���A��A��ZA���A��A��A�+�A�9�A�G�A�i�A˰UA�՛A���A���A�
rA��A�SA��A�.A��A��A�A�	lA�A��VA��A��`A��+A��vA���A��,Aʀ4A��A��gA�hsA�@�A� �A� �A�͟AǱ�A�H�A��A��?AŨ$AĀ A���A��A��
A�;�A�p�A���A�&A���A�  A�"A���A���A��	A�$@A�OvA�>�A��A��HA��RA�zDA�)�A���A�&�A��A��hA�M�A��2A��]A�W
A���A~)�As��Aq�An~�Ak�Aec A_4A\�hAXU�ATیAR5?AO�HAN;�AK� AH1�AF�tAEb�ABMA@e�A?��A?/A>�A<e�A;$�A; iA:Y�A9�AA9XyA9�A8��A8A�A6<�A0)�A/�DA/V�A/�eA0��A/��A/t�A.-wA-�cA-8�A,c A*6zA)A(;�A&hsA%4�A$6A#XA"�A"e,A!��A!XA!�eA":�A"X�A"^�A!�vA!�A e�A $tA��A[WA�A��A4nA�A��A4A�-A��A�{A�A�xA;Au�A��Ai�AOA �A|�A�CA
�A��A�AA;�AM�AHA!Av�A�oAW�A��AϫA�A�A;Am]A+A
�'A
`BA
�A	y>A	xA�SAy>AC�A҉A6zA��A�hAS&AqA�A��AXA�A�hA�A�A3�A�'A�}AO�A	�A �pA ��A Z�@�
=@�`�@��@��@�'�@��.@��o@�ff@��3@�|�@���@���@�1'@�Mj@���@�*�@��@��/@��@�Q@��@��@�ߤ@���@��N@�+@�]d@�e,@��/@셈@�7�@�7�@���@���@�@��@�!@��@�4@�M�@�tT@�S&@���@��@�z@�j@��@�x@���@��@�S&@���@�kQ@��@��@��;@�M@�8�@��@�r@�!@��.@��@�}�@�L�@�/�@�!-@��@�ߤ@�^5@�.�@���@�@���@߮�@�L0@��>@ݞ�@�zx@��@��T@۫�@ڶ�@ٝ�@ءb@���@�x�@��+@��H@� �@�;d@��@��'@ґ�@��d@� i@���@Ы6@���@Ϥ@@���@�P�@���@��[@̣@̜x@̕@̗�@̏\@�.�@˗$@�Y�@�!-@��@��8@ʯO@�Ft@��@�s�@��@ȼj@ȸR@ȴ9@ȱ�@ȸR@�u%@ǩ*@�Z�@�Mj@�?}@���@ƞ@�5?@���@Ŭq@Ŕ�@�a@�0�@�l�@�_p@��@��,@�t�@��@��w@��:@�:�@�Ĝ@���@�@�/�@��f@�m�@��@��@�u�@�{�@�	�@�o�@�!�@��@���@��P@��8@���@��+@��q@�J#@�6z@�+@��@��u@�~�@�`�@�GE@�oi@��o@��@�*0@��@�?�@�N�@�'R@��#@�@O@�%@��@���@�M�@�M@���@���@���@�qv@�RT@��@�I�@��@��;@���@�!�@��K@��)@���@�[�@�
�@��F@��~@�O@��@�@���@�O@���@�g�@�@O@��@�V@��@���@���@� �@��@���@�A�@��2@���@�:*@���@���@�n/@�k�@�]�@�+@�{�@���@�A @�Y@��@�V@��@��@�e�@��m@���@�K�@�/@��@���@���@��@�w�@�I�@�"h@��@��@��}@��@@�a�@�C@���@��+@��@�1@��@�w2@�+�@��@���@�)�@���@���@���@�k�@�S�@�8@���@���@�l�@�O@��@���@�@O@�ߤ@��h@��_@���@�h
@�	@���@��F@���@�n/@� i@���@��b@�c @���@��t@���@�rG@�)_@��!@�c @�,=@��K@�w2@��@���@�N�@�.�@�-�@�/�@��@��@��@�c�@��@��6@�J�@��@��z@���@��"@�qv@� \@���@�q@�'R@���@��z@���@���@�Vm@�?}@�@@��@��@�~(@�B[@�_@���@�>�@���@�ȴ@�� @�_@�'R@��a@�y�@��2@���@�tT@�.�@��g@�ƨ@���@�|�@�>�@��P@�Ɇ@��@�w�@�K^@�$�@��@���@���@���@�b�@���@��@���@�h
@��j@���@���@��S@���@��:@��4@�b�@�8�@��2@��h@���@�c @��@��@�V@��@��@\)@~ߤ@~�+@~{@}�=@}q@|�@|��@|�@|%�@{��@{�@z�8@z�@zں@z��@z�m@z�@z��@zL0@z�@y�@y��@y�@xA�@w��@w�$@wb�@w'�@v{�@u��@u��@uT�@u�@t��@tbN@s�@sb�@r�2@ra|@q�@p��@pl"@o�k@n�2@n�@m�7@mq@loi@k�@kqv@k@O@j�@je@i%@h�@gs@go@f�@f�]@f:*@ec@d�	@d��@dI�@d�@c�}@b�]@b
�@a�9@a\�@`I�@_�@_�}@_��@^z@^5?@]�>@]|@\�`@\�D@\H@[�r@[خ@[�w@[�k@[\)@[)_@Z�!@Z��@ZGE@Y|@Y%@X�@Wx@U�@U��@U}�@U=�@T�|@T��@T�o@TZ@T*�@T@S�@S�F@S�k@S��@Sj�@S\)@S�@Rv�@Q�~@QG�@P��@P�@O�[@Ox@Oe�@O1�@N��@N)�@M��@M�@L��@L��@Ll"@L,=@K�q@K�	@KK�@K+@J��@J��@J�A@J1�@I�@Im]@Iq@Hy>@G��@G�@G��@GX�@Fں@FJ@E��@E��@E�@E��@E�-@E�n@DĜ@C��@C��@C��@C9�@C�@B��@BE�@A�H@Au�@A;@@�.@@S�@?��@?s@?W?@?�@>�x@>B[@=��@=�h@=IR@=q@=%@<��@<,=@<x@;��@;�@;x@;y�@;iD@;@O@:ں@:~�@:&�@9�@9�'@9�S@9�"@9`B@9/@8�P@8�E@8�9@8�@8N�@8x@7خ@7��@7A�@6�h@6@�@6&�@64@5�@5�N@5��@5L�@4Ɇ@4��@4�Y@4,=@3˒@3J#@3!-@3�@2�@2�'@2�@2��@2a|@1�j@1m]@1q@0�U@0�@0��@0u�@06@02�@/˒@/�@.��@.?@.?@.1�@.e@-�.@-��@-c@-hs@-[W@-O�@-F@-?}@-5�@-%@,>B@+A�@*�"@*��@*�'@*�@*}V@*R�@*�@)��@)�t@)��@)��@)��@)��@)��@)e,@)&�@(ی@(6@'F�@'$t@&��@&�}@&~�@&R�@&6�@&�@%��@%��@%��@%�n@%�h@%��@%[W@%@@$�f@$�@$z�@#�+@#خ@#��@#��@#b�@#e�@#t�@#x@#v`@#qv@#A�@#S@"�m@"J�@!��@!s�@!?}@!#�@!�@!@ �@ ��@ �@ `�@ �@�@�;@��@y�@b�@H�@
=@��@��@��@p;@3�@_@�#@Q�@�K@�[@ѷ@�@��@�o@e�@7�@  @�	@W?@>�@1�@�@�]@�+@0U@�@��@��@T�@	l@��@*�@�@�}@��@C@�@҉@ȴ@��@��@�+@��@u%@_�@J�@6�@ �@�9@�@��@�h@e,@0�@(�@ \@�@֡@|�@(�@  @خ@�*@��@��@x@dZ@Mj@��@�b@�\@d�@H�@5?@{@��@�N@c@=�@%F@�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B[WB[	BZ�B[�B\B`'B`�Ba�Bd�BiBl�Bo�BxlB|�B�OB�lB�xB��B�B��B�`B��B�B��B�qB��B�B�B�B�B�(B�]B�BB�BB�]B�cB�BּB�	B�;B	}B	�eB	�9B	�TB	�VB
NB
:�B
�B
�CB
�B
�;B
�LBB%�B6�B[	Bb�BvzB�PB��Bz�BZ�BH1BtB
�hB
��B
��B
�
B
�-B
R�B
;�B
.�B
*�B
$B
�B
NB	��B	�eB	ܬB	�B
AUB	�MB	�qB	��B	{JB	e�B	FYB	"4B	�B	�B�B��B�~B��B�B�JB�hB�B�`B��B��B�_B��B��B��B�"BуB�(B޸B�8B	�B	�B	33B	B	!bB	&�B	1�B	W�B	U2B	\�B	c�B	mCB	n�B	m]B	i�B	a�B	]~B	I�B	<B	:�B	A�B	G�B	LB	Q4B	^jB	B	��B	�B	�"B	�jB	��B	��B	��B	�<B	�NB	��B	�/B	�_B	�B	��B	�
B	�B	��B	�CB	�QB	��B	�IB	��B	��B	ߊB	�2B	�LB	�B	�KB	�B	�B	�B	�LB	�B	�B	�B	��B	��B	�wB	�B	�mB	�B	�B	�B	�XB	�$B	�mB	�>B	��B	�B	�B	�B	�B	�B	�&B	�|B	��B	��B	�nB	��B	�B	��B	�B	�|B	�4B	�B	�B	�B	�B	�TB	��B	�B	�nB	�ZB	�&B	�B	��B	��B	�HB	�-B	�BB	�'B	�\B	�:B	�B	�B	�,B	�B	�B	��B	��B	��B	�B	�B	�B	�|B	�B	�B	�hB	�B	�nB	��B	��B	��B	�nB	�nB	�fB	��B	�B	�B	�"B	�B	��B	�3B	�FB	��B	��B
 OB
 B
 �B
oB
{B
gB
B
�B
[B
UB
;B
�B
�B
_B
EB
�B
�B
?B
9B
9B
9B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	RB
�B
�B
�B
�B
B
{B
-B
�B
B
 �B	��B	�B	�rB	��B	�?B	�-B	�cB	�"B	�KB	�_B	��B	�>B	�B	�ZB	�2B	�RB	�RB	�2B	��B	�2B	�,B	��B	��B	�yB	�B	�B	�B	�OB	�-B	�|B	��B	�B	��B	�MB	�MB	�TB	�`B	�B	��B	��B	��B	��B	��B	��B	�B	�FB	��B	��B	�8B	�B	�RB	��B	��B	�^B	��B	��B	��B	�B	�JB	��B	�RB	��B	�B	��B	��B	�B	�nB	��B	�MB	�[B	�'B	�AB	�GB	�MB	�B	��B	��B	�9B	�FB	��B	��B	��B	�	B	��B	�RB	�fB	��B	�LB	��B	�0B	�B	�6B	��B	��B	��B	�(B	��B	��B	��B	�]B
 �B
[B
[B
[B
-B
�B
GB
�B
aB
{B
-B
�B
B
{B
�B
B
�B
�B
3B
�B
B
MB
B
�B
B
�B
gB
gB
gB
mB
�B
+B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	7B
	RB

#B

�B

�B

XB

#B

	B

�B
�B

�B

�B

�B

�B

�B

�B

�B
^B
�B
�B
6B
jB
B
(B
.B
�B
�B
hB
@B
�B
�B
�B
�B
B
FB
�B
[B
TB
�B
�B
(B
�B
\B
�B
B
HB
bB
�B
}B
�B
�B
 B
�B
�B
 B
�B
uB
�B
�B
�B
B
,B
aB
MB
�B
B
B
B
9B
9B
SB
mB
?B
�B
EB
yB
+B
�B
sB
?B
�B
EB
yB
�B
eB
�B
�B
B
QB
�B
�B
	B
	B
=B
�B
]B
�B
�B
�B
�B
]B
�B
�B
dB
~B
~B
�B
�B
VB
VB
�B
 B
�B
 B
 �B
 �B
!�B
"B
"�B
"�B
#nB
#�B
$&B
$�B
$�B
&LB
&�B
&LB
'B
'�B
'�B
($B
(sB
(�B
)�B
*�B
+kB
+�B
,B
,�B
,�B
,�B
-)B
-B
-�B
-�B
-�B
-wB
-�B
-�B
-�B
.B
.cB
/OB
0UB
0�B
1B
1AB
1�B
1[B
1�B
1�B
1vB
1AB
1'B
1AB
1vB
2B
2B
2-B
2GB
2|B
2�B
2�B
2�B
2GB
2�B
2�B
3MB
33B
3MB
3hB
3hB
3hB
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
5B
5B
5�B
6+B
6FB
6�B
7B
72B
7�B
8B
9	B
9�B
:*B
:�B
;B
;�B
;�B
<6B
<jB
<�B
<�B
="B
=�B
=�B
=�B
>B
>(B
>�B
>�B
?cB
?}B
?HB
?HB
?�B
@ B
?�B
?�B
?}B
?cB
?HB
?�B
?�B
?�B
?�B
@B
@B
?�B
@ B
@�B
@�B
@�B
@�B
A B
A�B
B'B
B[B
B[B
B�B
B�B
B�B
B�B
C-B
CB
B�B
CB
B�B
B�B
CaB
D3B
DB
DMB
DMB
D�B
D�B
D�B
D�B
EB
EB
EB
E9B
ESB
ESB
EmB
E9B
ESB
E�B
F�B
F�B
GEB
G�B
G�B
G�B
G�B
G�B
HB
H�B
IlB
I�B
J	B
J	B
J	B
J	B
JXB
J=B
J�B
J�B
KB
J�B
K)B
K�B
K�B
LB
LB
LdB
L~B
L�B
L�B
L�B
MPB
N"B
NVB
NVB
NpB
N�B
NpB
NpB
OvB
O�B
O�B
O�B
PbB
P�B
P�B
QNB
Q�B
Q�B
RoB
R�B
R�B
S�B
S�B
S�B
TB
S�B
S�B
TaB
T{B
T�B
U2B
UMB
VB
W$B
WYB
W�B
XB
XB
XB
W�B
X+B
X_B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\B
[�B
\B
\]B
\�B
\�B
]B
]/B
]~B
^B
^B
^B
^5B
^jB
^�B
^jB
^�B
_!B
_;B
_�B
_�B
_�B
_�B
_�B
`B
_�B
`vB
`�B
aHB
a�B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cnB
dZB
d@B
d@B
dtB
d�B
d�B
d�B
d�B
eB
e`B
eFB
e`B
eFB
e,B
eFB
eFB
ezB
e�B
f2B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h>B
hXB
hXB
hsB
h�B
hsB
h�B
h�B
h�B
h�B
i*B
iyB
i�B
i�B
i�B
jB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
f�B
k�B
k�B
lB
lB
l"B
l=B
lWB
lqB
l�B
mB
mB
m)B
m]B
mwB
mwB
m�B
m�B
m�B
nB
n/B
ncB
n�B
n�B
n�B
oOB
o�B
o�B
o�B
pB
pB
o�B
pB
pB
p;B
p�B
p�B
p�B
p�B
qB
qB
qvB
q�B
q�B
q�B
q�B
r|B
r�B
r�B
shB
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u%B
u%B
utB
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v+B
vB
vzB
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xRB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y�B
y�B
y�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B[WB[	BZ�B[�B\B`'B`�Ba�Bd�BiBl�Bo�BxlB|�B�OB�lB�xB��B�B��B�`B��B�B��B�qB��B�B�B�B�B�(B�]B�BB�BB�]B�cB�BּB�	B�;B	}B	�eB	�9B	�TB	�VB
NB
:�B
�B
�CB
�B
�;B
�LBB%�B6�B[	Bb�BvzB�PB��Bz�BZ�BH1BtB
�hB
��B
��B
�
B
�-B
R�B
;�B
.�B
*�B
$B
�B
NB	��B	�eB	ܬB	�B
AUB	�MB	�qB	��B	{JB	e�B	FYB	"4B	�B	�B�B��B�~B��B�B�JB�hB�B�`B��B��B�_B��B��B��B�"BуB�(B޸B�8B	�B	�B	33B	B	!bB	&�B	1�B	W�B	U2B	\�B	c�B	mCB	n�B	m]B	i�B	a�B	]~B	I�B	<B	:�B	A�B	G�B	LB	Q4B	^jB	B	��B	�B	�"B	�jB	��B	��B	��B	�<B	�NB	��B	�/B	�_B	�B	��B	�
B	�B	��B	�CB	�QB	��B	�IB	��B	��B	ߊB	�2B	�LB	�B	�KB	�B	�B	�B	�LB	�B	�B	�B	��B	��B	�wB	�B	�mB	�B	�B	�B	�XB	�$B	�mB	�>B	��B	�B	�B	�B	�B	�B	�&B	�|B	��B	��B	�nB	��B	�B	��B	�B	�|B	�4B	�B	�B	�B	�B	�TB	��B	�B	�nB	�ZB	�&B	�B	��B	��B	�HB	�-B	�BB	�'B	�\B	�:B	�B	�B	�,B	�B	�B	��B	��B	��B	�B	�B	�B	�|B	�B	�B	�hB	�B	�nB	��B	��B	��B	�nB	�nB	�fB	��B	�B	�B	�"B	�B	��B	�3B	�FB	��B	��B
 OB
 B
 �B
oB
{B
gB
B
�B
[B
UB
;B
�B
�B
_B
EB
�B
�B
?B
9B
9B
9B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	RB
�B
�B
�B
�B
B
{B
-B
�B
B
 �B	��B	�B	�rB	��B	�?B	�-B	�cB	�"B	�KB	�_B	��B	�>B	�B	�ZB	�2B	�RB	�RB	�2B	��B	�2B	�,B	��B	��B	�yB	�B	�B	�B	�OB	�-B	�|B	��B	�B	��B	�MB	�MB	�TB	�`B	�B	��B	��B	��B	��B	��B	��B	�B	�FB	��B	��B	�8B	�B	�RB	��B	��B	�^B	��B	��B	��B	�B	�JB	��B	�RB	��B	�B	��B	��B	�B	�nB	��B	�MB	�[B	�'B	�AB	�GB	�MB	�B	��B	��B	�9B	�FB	��B	��B	��B	�	B	��B	�RB	�fB	��B	�LB	��B	�0B	�B	�6B	��B	��B	��B	�(B	��B	��B	��B	�]B
 �B
[B
[B
[B
-B
�B
GB
�B
aB
{B
-B
�B
B
{B
�B
B
�B
�B
3B
�B
B
MB
B
�B
B
�B
gB
gB
gB
mB
�B
+B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	7B
	RB

#B

�B

�B

XB

#B

	B

�B
�B

�B

�B

�B

�B

�B

�B

�B
^B
�B
�B
6B
jB
B
(B
.B
�B
�B
hB
@B
�B
�B
�B
�B
B
FB
�B
[B
TB
�B
�B
(B
�B
\B
�B
B
HB
bB
�B
}B
�B
�B
 B
�B
�B
 B
�B
uB
�B
�B
�B
B
,B
aB
MB
�B
B
B
B
9B
9B
SB
mB
?B
�B
EB
yB
+B
�B
sB
?B
�B
EB
yB
�B
eB
�B
�B
B
QB
�B
�B
	B
	B
=B
�B
]B
�B
�B
�B
�B
]B
�B
�B
dB
~B
~B
�B
�B
VB
VB
�B
 B
�B
 B
 �B
 �B
!�B
"B
"�B
"�B
#nB
#�B
$&B
$�B
$�B
&LB
&�B
&LB
'B
'�B
'�B
($B
(sB
(�B
)�B
*�B
+kB
+�B
,B
,�B
,�B
,�B
-)B
-B
-�B
-�B
-�B
-wB
-�B
-�B
-�B
.B
.cB
/OB
0UB
0�B
1B
1AB
1�B
1[B
1�B
1�B
1vB
1AB
1'B
1AB
1vB
2B
2B
2-B
2GB
2|B
2�B
2�B
2�B
2GB
2�B
2�B
3MB
33B
3MB
3hB
3hB
3hB
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
5B
5B
5�B
6+B
6FB
6�B
7B
72B
7�B
8B
9	B
9�B
:*B
:�B
;B
;�B
;�B
<6B
<jB
<�B
<�B
="B
=�B
=�B
=�B
>B
>(B
>�B
>�B
?cB
?}B
?HB
?HB
?�B
@ B
?�B
?�B
?}B
?cB
?HB
?�B
?�B
?�B
?�B
@B
@B
?�B
@ B
@�B
@�B
@�B
@�B
A B
A�B
B'B
B[B
B[B
B�B
B�B
B�B
B�B
C-B
CB
B�B
CB
B�B
B�B
CaB
D3B
DB
DMB
DMB
D�B
D�B
D�B
D�B
EB
EB
EB
E9B
ESB
ESB
EmB
E9B
ESB
E�B
F�B
F�B
GEB
G�B
G�B
G�B
G�B
G�B
HB
H�B
IlB
I�B
J	B
J	B
J	B
J	B
JXB
J=B
J�B
J�B
KB
J�B
K)B
K�B
K�B
LB
LB
LdB
L~B
L�B
L�B
L�B
MPB
N"B
NVB
NVB
NpB
N�B
NpB
NpB
OvB
O�B
O�B
O�B
PbB
P�B
P�B
QNB
Q�B
Q�B
RoB
R�B
R�B
S�B
S�B
S�B
TB
S�B
S�B
TaB
T{B
T�B
U2B
UMB
VB
W$B
WYB
W�B
XB
XB
XB
W�B
X+B
X_B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\B
[�B
\B
\]B
\�B
\�B
]B
]/B
]~B
^B
^B
^B
^5B
^jB
^�B
^jB
^�B
_!B
_;B
_�B
_�B
_�B
_�B
_�B
`B
_�B
`vB
`�B
aHB
a�B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
cnB
dZB
d@B
d@B
dtB
d�B
d�B
d�B
d�B
eB
e`B
eFB
e`B
eFB
e,B
eFB
eFB
ezB
e�B
f2B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h>B
hXB
hXB
hsB
h�B
hsB
h�B
h�B
h�B
h�B
i*B
iyB
i�B
i�B
i�B
jB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
f�B
k�B
k�B
lB
lB
l"B
l=B
lWB
lqB
l�B
mB
mB
m)B
m]B
mwB
mwB
m�B
m�B
m�B
nB
n/B
ncB
n�B
n�B
n�B
oOB
o�B
o�B
o�B
pB
pB
o�B
pB
pB
p;B
p�B
p�B
p�B
p�B
qB
qB
qvB
q�B
q�B
q�B
q�B
r|B
r�B
r�B
shB
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u%B
u%B
u%B
utB
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v+B
vB
vzB
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xRB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y�B
y�B
y�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104950  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175109  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175109  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175109                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025117  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025117  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                