CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:28:50Z creation;2022-06-04T19:28:51Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604192850  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٥�z��1   @٥���I�@+��n���d#��$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���A�ffBffB  B33B��B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B���B�  B�  B�  B�  B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C33C��C�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(33C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�z�A�zB=pB�
B
=Bp�B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bh=pBp=pBw�
B�
B��B��B��B��B��B��B��RB��B��RB��RB��B��B��B��B��B��B��B��B�Q�B�Q�B��B׸RB��B��B��B��B��B��B��B��B��B��C��C(�CC�)C	�)C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C((�C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf]Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL��DL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw��Dw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�;�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A��A��mA��NA�� A��A��A��DA��A���A��A��yA��A���A���A���A��QA��A��A��/A���A��vA��/A���A�iyA�#�A�(XA���A�XEA��A��>A���A�qA��A�F?A�A�@A�zxAº�A�=<A��^A�N<A��A���A��{A�̘A�Q�A��5A�6FA���A�^�A���A��FA��BA�>�A�lWA�֡A��A�	�A��#A�"�A��sA��RA���A�-�A�U�A��\A���A��A��A��A�j�A�!�A��A��]A�X�A�[�A���A��,Ap;Az<6Au*�Aq�&Ao�9Am�Aj�FAh  Ae<�Aa4A]�RA[rGAY��AW�ASdZAR_�AP��AN	AK��AJ@AI�AG�AF:�AC��AA}VA?e�A=��A<�XA:C�A9+A6�7A2��A1�}A/n/A,TaA*i�A'OvA& �A%c�A$�/A$E9A"�A"�A�NA��A7�A�~A8�A�A[WA��A��A��A��A�AȴA��A$�Az�AsA�AK^A�2Ab�A:*A�A2aA�$A[�A"hA��A�XAjA<�A	lAA A�
A�A�vA�AیA��Ae�A?A�AخA�bAffA"hA
��A	��A	)�A��AtTA�[A��A��A4nAĜA��AO�A�,ADgA��A	A��A:�An/A{A aA O�A RT@��@�@�e�@���@�e@���@��q@��z@���@��@�9X@�;d@��@���@�G@���@�֡@���@��X@��@�dZ@��@�"@�L@�@���@�ی@�(@�[�@�F@�\�@��A@�֡@�F�@�qv@���@�_@�� @��@�I�@�hs@�@�Vm@���@�(�@��@��@��&@��@�>�@�+@��@��X@��P@�`B@��@�v�@��@�q@�/�@��'@��@�H�@��@ޟ�@�خ@�T�@��@��v@��P@�Ɇ@ܓu@�r�@�H�@�M@ۈf@��@ں�@�d�@���@٢�@ـ4@�W?@�%F@���@�q�@��@֠�@��@�c�@��X@�K^@ӓ�@�*0@��@�l"@���@�:�@��@й$@�v�@�u�@�L0@���@�}�@��@θR@�h
@�@͟V@�Y@�R�@�/�@�@���@�~�@�B�@��"@ʭ�@�Ov@��)@�L�@��K@�1�@ǥ�@�;d@��E@�y>@�B[@���@ŗ$@�X@��@ħ�@�7�@�@���@��@�~�@�[W@�1�@��@���@�@��@��a@��k@�U�@��K@��}@��@���@��@���@��"@�r�@�'R@��@��@���@���@�x�@�=�@�%F@���@�Xy@��@�g�@��@��@�4@���@�+�@�%�@���@�A @��8@�p;@���@���@��u@���@�˒@��w@��	@�#�@�s�@�?�@� �@��C@�#�@�/�@��W@��@�y�@�Dg@��E@�p;@�@��@��{@�A @���@�,=@��@�@���@�=�@�a|@���@�K�@��`@���@�\�@���@��~@�S&@��@�%@���@���@�j@�9X@��q@��@��@�V@�	�@�c@�O�@��@���@�b@�ݘ@�~�@��E@�q@�$@��@��)@�@��@�w2@�q@���@�#:@��@�Vm@�RT@�5�@��,@�}V@�@��0@��M@�+�@�@�ѷ@�C-@�خ@�O@��2@��!@�{�@�%�@��3@�~�@��@���@�_@���@���@�/�@�͟@���@�&�@��T@���@�s@��@���@��6@�Z�@�x@��@��0@��4@�Mj@�8@���@��6@�\�@�	@��A@�˒@���@�4@��@��j@��@�kQ@��@��D@�{J@�8�@�*0@��@��F@�K^@�($@�e@���@���@�zx@�X@�>�@���@��@���@���@�j@�~@���@��Q@��[@��h@��7@��4@�u�@�j�@�^�@�8@��@��)@��$@��O@��u@��@�=q@�7@��A@�s@�?}@�S@���@��.@�5?@�_@��@��@���@�S�@��@�Xy@�+k@�J@W?@~��@~��@~
�@}�n@}Dg@|�@| �@|M@{��@{t�@z�@z6�@y�3@y2a@x�@x�U@x��@x��@xh�@x%�@w��@wqv@w1�@w�@w�@v�y@v�<@v��@vd�@v!�@u�z@uj@uT�@u2a@t��@t��@t|�@s]�@sS@r��@r�,@rkQ@q�C@qm]@q%F@p��@p�@o��@ov`@oZ�@oU�@o.I@n��@n�A@m��@m�N@m�@mzx@mDg@l�P@l�4@l�_@l�u@ltT@l4n@l1@k�4@j��@j4@i��@i�^@i��@i�7@i=�@i�@h��@gn/@f�h@f�1@f��@f@�@e��@e�@dK^@cZ�@bߤ@bh
@b�@a�@a�'@a\�@a=�@aV@`A�@_s@_O@^�@^�1@^�@^@]�D@]�t@]+@\�|@\��@\�.@\w�@\C-@[�@Z��@Zs�@ZYK@Yk�@Xی@X��@X`�@X�@W�&@W�{@W�@V��@Vp;@U��@U�C@UF@UV@Tѷ@T"h@S��@SU�@R��@R.�@Qk�@Q�@P��@PM@O��@OC�@N�@N��@N6�@M��@L�@L��@LbN@L�@Kl�@K@Jp;@JTa@J-@I�@I��@H�o@Gg�@F��@F��@FkQ@F@�@F�@E�j@E��@E�@E��@Ehs@EX@ES&@E(�@E&�@E�@D�p@DV�@D<�@D*�@Dx@C��@C�k@Cg�@C+@B��@B��@Bh
@B#:@A�@Au�@AVm@A \@@bN@?�@?H�@?�@>��@>8�@>_@=�3@=N<@<�|@<�_@<l"@<"h@<�@;�@;g�@;�@:҉@:��@:!�@9�j@9��@9x�@9`B@9F@9:�@9!�@9	l@8�@8֡@8�$@8��@8m�@8b@7��@7�f@7U�@7(@6��@6B[@5�H@50�@4�@4�@4_@4-�@3��@2�@2d�@1�@1`B@0��@0$@/�@/�$@/��@/s@/s@/W?@/9�@/�@/S@.��@.�@.�s@.��@.kQ@.e@-��@-k�@,�	@,��@,tT@,�@+��@+K�@+�@+�@*��@*�6@)��@(�P@(Ɇ@(�E@(�O@(C-@'�K@'�@'O@'o@&�8@&��@&�@%�#@%}�@%�@$j@$<�@#�+@#�;@#�:@#�@"��@"�X@"�\@"6�@!�H@!��@!�7@!T�@!#�@ ��@ ֡@ �z@�Q@A�@6z@�@��@�1@v�@+k@$�@	@�@�#@s�@5�@�`@�_@l"@:�@1@��@iD@C�@�@�h@�@L0@�@��@��@w2@[W@�@�?@�o@Xy@�@�g@��@�@�{@J#@C@ȴ@��@h
@J�@
�@	@_@�@��@�@��@zx@A @-w@�@��@��@c�@C-@-�@M@��@�@@�	@A�@�@�'@�x@ff@0U@ �@�@�>@�9@�-@�~@k�@4@(�@�f@�$@�D@]d@(�@��@�@��@�@��@��@J#@�@v�@V@-@�@��@w2@e,@B�@�@ѷ@�?@�4@�Y@c�@K^@Ft@Ft@D�@2�@�&@�@o�@RT@@
�<@
�\@
l�@
d�@
:*@
�@	��@	��@	��@	N<@��@�O@q@V�@?�@,=@��@�m@�K@��@�@��@g�@@�X@ff@E�@
�@�@��@Q�@2a@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A��A��mA��NA�� A��A��A��DA��A���A��A��yA��A���A���A���A��QA��A��A��/A���A��vA��/A���A�iyA�#�A�(XA���A�XEA��A��>A���A�qA��A�F?A�A�@A�zxAº�A�=<A��^A�N<A��A���A��{A�̘A�Q�A��5A�6FA���A�^�A���A��FA��BA�>�A�lWA�֡A��A�	�A��#A�"�A��sA��RA���A�-�A�U�A��\A���A��A��A��A�j�A�!�A��A��]A�X�A�[�A���A��,Ap;Az<6Au*�Aq�&Ao�9Am�Aj�FAh  Ae<�Aa4A]�RA[rGAY��AW�ASdZAR_�AP��AN	AK��AJ@AI�AG�AF:�AC��AA}VA?e�A=��A<�XA:C�A9+A6�7A2��A1�}A/n/A,TaA*i�A'OvA& �A%c�A$�/A$E9A"�A"�A�NA��A7�A�~A8�A�A[WA��A��A��A��A�AȴA��A$�Az�AsA�AK^A�2Ab�A:*A�A2aA�$A[�A"hA��A�XAjA<�A	lAA A�
A�A�vA�AیA��Ae�A?A�AخA�bAffA"hA
��A	��A	)�A��AtTA�[A��A��A4nAĜA��AO�A�,ADgA��A	A��A:�An/A{A aA O�A RT@��@�@�e�@���@�e@���@��q@��z@���@��@�9X@�;d@��@���@�G@���@�֡@���@��X@��@�dZ@��@�"@�L@�@���@�ی@�(@�[�@�F@�\�@��A@�֡@�F�@�qv@���@�_@�� @��@�I�@�hs@�@�Vm@���@�(�@��@��@��&@��@�>�@�+@��@��X@��P@�`B@��@�v�@��@�q@�/�@��'@��@�H�@��@ޟ�@�خ@�T�@��@��v@��P@�Ɇ@ܓu@�r�@�H�@�M@ۈf@��@ں�@�d�@���@٢�@ـ4@�W?@�%F@���@�q�@��@֠�@��@�c�@��X@�K^@ӓ�@�*0@��@�l"@���@�:�@��@й$@�v�@�u�@�L0@���@�}�@��@θR@�h
@�@͟V@�Y@�R�@�/�@�@���@�~�@�B�@��"@ʭ�@�Ov@��)@�L�@��K@�1�@ǥ�@�;d@��E@�y>@�B[@���@ŗ$@�X@��@ħ�@�7�@�@���@��@�~�@�[W@�1�@��@���@�@��@��a@��k@�U�@��K@��}@��@���@��@���@��"@�r�@�'R@��@��@���@���@�x�@�=�@�%F@���@�Xy@��@�g�@��@��@�4@���@�+�@�%�@���@�A @��8@�p;@���@���@��u@���@�˒@��w@��	@�#�@�s�@�?�@� �@��C@�#�@�/�@��W@��@�y�@�Dg@��E@�p;@�@��@��{@�A @���@�,=@��@�@���@�=�@�a|@���@�K�@��`@���@�\�@���@��~@�S&@��@�%@���@���@�j@�9X@��q@��@��@�V@�	�@�c@�O�@��@���@�b@�ݘ@�~�@��E@�q@�$@��@��)@�@��@�w2@�q@���@�#:@��@�Vm@�RT@�5�@��,@�}V@�@��0@��M@�+�@�@�ѷ@�C-@�خ@�O@��2@��!@�{�@�%�@��3@�~�@��@���@�_@���@���@�/�@�͟@���@�&�@��T@���@�s@��@���@��6@�Z�@�x@��@��0@��4@�Mj@�8@���@��6@�\�@�	@��A@�˒@���@�4@��@��j@��@�kQ@��@��D@�{J@�8�@�*0@��@��F@�K^@�($@�e@���@���@�zx@�X@�>�@���@��@���@���@�j@�~@���@��Q@��[@��h@��7@��4@�u�@�j�@�^�@�8@��@��)@��$@��O@��u@��@�=q@�7@��A@�s@�?}@�S@���@��.@�5?@�_@��@��@���@�S�@��@�Xy@�+k@�J@W?@~��@~��@~
�@}�n@}Dg@|�@| �@|M@{��@{t�@z�@z6�@y�3@y2a@x�@x�U@x��@x��@xh�@x%�@w��@wqv@w1�@w�@w�@v�y@v�<@v��@vd�@v!�@u�z@uj@uT�@u2a@t��@t��@t|�@s]�@sS@r��@r�,@rkQ@q�C@qm]@q%F@p��@p�@o��@ov`@oZ�@oU�@o.I@n��@n�A@m��@m�N@m�@mzx@mDg@l�P@l�4@l�_@l�u@ltT@l4n@l1@k�4@j��@j4@i��@i�^@i��@i�7@i=�@i�@h��@gn/@f�h@f�1@f��@f@�@e��@e�@dK^@cZ�@bߤ@bh
@b�@a�@a�'@a\�@a=�@aV@`A�@_s@_O@^�@^�1@^�@^@]�D@]�t@]+@\�|@\��@\�.@\w�@\C-@[�@Z��@Zs�@ZYK@Yk�@Xی@X��@X`�@X�@W�&@W�{@W�@V��@Vp;@U��@U�C@UF@UV@Tѷ@T"h@S��@SU�@R��@R.�@Qk�@Q�@P��@PM@O��@OC�@N�@N��@N6�@M��@L�@L��@LbN@L�@Kl�@K@Jp;@JTa@J-@I�@I��@H�o@Gg�@F��@F��@FkQ@F@�@F�@E�j@E��@E�@E��@Ehs@EX@ES&@E(�@E&�@E�@D�p@DV�@D<�@D*�@Dx@C��@C�k@Cg�@C+@B��@B��@Bh
@B#:@A�@Au�@AVm@A \@@bN@?�@?H�@?�@>��@>8�@>_@=�3@=N<@<�|@<�_@<l"@<"h@<�@;�@;g�@;�@:҉@:��@:!�@9�j@9��@9x�@9`B@9F@9:�@9!�@9	l@8�@8֡@8�$@8��@8m�@8b@7��@7�f@7U�@7(@6��@6B[@5�H@50�@4�@4�@4_@4-�@3��@2�@2d�@1�@1`B@0��@0$@/�@/�$@/��@/s@/s@/W?@/9�@/�@/S@.��@.�@.�s@.��@.kQ@.e@-��@-k�@,�	@,��@,tT@,�@+��@+K�@+�@+�@*��@*�6@)��@(�P@(Ɇ@(�E@(�O@(C-@'�K@'�@'O@'o@&�8@&��@&�@%�#@%}�@%�@$j@$<�@#�+@#�;@#�:@#�@"��@"�X@"�\@"6�@!�H@!��@!�7@!T�@!#�@ ��@ ֡@ �z@�Q@A�@6z@�@��@�1@v�@+k@$�@	@�@�#@s�@5�@�`@�_@l"@:�@1@��@iD@C�@�@�h@�@L0@�@��@��@w2@[W@�@�?@�o@Xy@�@�g@��@�@�{@J#@C@ȴ@��@h
@J�@
�@	@_@�@��@�@��@zx@A @-w@�@��@��@c�@C-@-�@M@��@�@@�	@A�@�@�'@�x@ff@0U@ �@�@�>@�9@�-@�~@k�@4@(�@�f@�$@�D@]d@(�@��@�@��@�@��@��@J#@�@v�@V@-@�@��@w2@e,@B�@�@ѷ@�?@�4@�Y@c�@K^@Ft@Ft@D�@2�@�&@�@o�@RT@@
�<@
�\@
l�@
d�@
:*@
�@	��@	��@	��@	N<@��@�O@q@V�@?�@,=@��@�m@�K@��@�@��@g�@@�X@ff@E�@
�@�@��@Q�@2a@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	uB	u%B	uZB	uZB	u?B	uB	tnB	t9B	t9B	tnB	tnB	t�B	tTB	t�B	t�B	tnB	t�B	t�B	uB	t�B	t�B	t�B	tnB	u�B	��B	�DB	��B
1�B
j�B
g�B
�FBGBAoBZ�B[�BQ BB'B�B�B
�B�B�B+B@BBABH1Ba�BoiB|�B��B�zBҽB�B��B��B�B�B�B B�$B�xB��BچB�vB�VBɠB�oB�@Bv�BESB+QB 'BmB
� B
��B
�mB
��B
z�B
^�B
NB
>�B
0�B
;B
oB	�B	�B	�HB	��B	��B	�6B	�eB	HB	g�B	Y�B	PbB	E�B	72B	.�B	(�B	 vB	�B	bB	jB	�B	-B��B��B�FB��B�0B	B	1B	�B	uB	 �B	�B	�B	[B��B	B	2-B	:*B	@�B	G�B	R�B	ZB	YKB	bB	c�B	c B	a�B	c B	`�B	j�B	j0B	e�B	pB	v�B	y>B	v�B	xB	~�B	}B	��B	�;B	�B	�rB	��B	�<B	�\B	�,B	��B	�eB	��B	��B	��B	��B	�xB	�B	��B	��B	��B	�B	��B	�B	��B	�zB	��B	�+B	�B	��B	�?B	�B	�`B	��B	�<B	ĜB	�MB	�IB	�B	�TB	��B	�B	��B	�B	��B	��B	�B	޸B	ںB	�sB	�B	�KB	ܬB	��B	چB	��B	�/B	�pB	�HB	޸B	�5B	��B	�IB	�dB	�/B	ܬB	�B	�B	��B	�nB	�B	�2B	�B	�B	�B	�B	��B	� B	�B	�B	�B	�B	�8B	�ZB	��B	�B	�
B	��B	�?B	�JB	�B	�B	��B	��B	��B	�+B	��B	��B	�B	�B	��B	�AB	��B	��B	�;B	�;B	��B	��B	�JB	��B	��B	��B	��B	�`B	�B	�B	�UB	�iB	�}B	��B	�B	�|B	�%B	��B	��B	��B	�B	�LB	�B	�lB	�	B	�XB	�>B	��B	��B	�(B	�B	��B	��B	�DB	�*B	�0B	�B	�B	�wB	��B	�jB	�B	�B	��B	�*B	��B	��B	�^B	��B	�B	�jB	��B	�6B	��B	�dB	��B	�B	��B	�DB	��B	�^B	��B	��B	�0B	�0B	��B	��B	��B	�0B	�xB	��B	�B	��B	�B	��B	�JB	�JB	��B	��B	�xB	�DB	�$B	��B	�JB	�dB	��B	��B	�0B	��B	�<B	�.B
 4B
 �B
 B
 4B	��B	�]B
  B
 4B	��B	�HB	��B	�B	��B	�BB	��B	��B
 OB
 �B
B
�B
�B
�B
aB
GB
�B
gB
�B
�B
B
�B
B
B
B
EB
�B
1B
fB
�B
�B
�B
�B
�B
	�B
	lB
	lB
	�B

�B

�B

rB

�B

rB
)B
�B
JB
B
B
6B
B
6B
�B
�B
�B
6B
jB
�B
B
VB
�B
�B
�B
bB
�B
�B
B
B
hB
NB
�B
�B
�B
[B
�B
B
aB
MB
MB
�B
9B
�B
�B
YB
�B
�B
EB
EB
EB
�B
yB
�B
1B
1B
KB
�B
kB
#B
�B
]B
�B
/B
dB
IB
dB
�B
�B
�B
�B
�B
5B
OB
�B
5B
OB
�B
!B
pB
pB
 BB
 �B
!B
!bB
!|B
"NB
"�B
"�B
"�B
#TB
#nB
#�B
$@B
$tB
$tB
$�B
$�B
%FB
%,B
%`B
%zB
%�B
&B
&2B
&fB
&fB
&fB
&�B
&�B
&�B
'B
'8B
'8B
'�B
(�B
)�B
)DB
)�B
*�B
,"B
-)B
-wB
-�B
.cB
.�B
.�B
/5B
/5B
/5B
/iB
/iB
0B
/�B
0!B
0�B
0�B
0�B
1B
1AB
1AB
1[B
1�B
1�B
2-B
2GB
2|B
2�B
2�B
3�B
3�B
3�B
4�B
4�B
5%B
5ZB
5�B
5�B
6+B
6+B
6`B
6�B
6�B
6�B
8B
7�B
7�B
8�B
8�B
8�B
9$B
9>B
9XB
:DB
:DB
:B
:DB
:DB
;B
;B
;0B
;�B
<B
<B
<B
<B
<jB
<jB
<jB
<�B
<�B
<�B
<�B
<�B
=B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@ B
@4B
@�B
@�B
AB
@�B
@�B
AB
A B
AoB
BB
BB
BAB
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C�B
D�B
D�B
D�B
D�B
EB
EB
E9B
ESB
E9B
F�B
G+B
G+B
G+B
GEB
G�B
G�B
HfB
IB
I7B
IlB
IlB
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J#B
J�B
J�B
J�B
KB
K^B
KxB
KxB
K�B
K�B
K�B
L0B
MB
MB
MB
N"B
NVB
N<B
NpB
N�B
N�B
N�B
N�B
OB
O\B
O�B
O�B
PB
P.B
PHB
P�B
Q4B
QNB
Q�B
RTB
R�B
SB
S[B
S@B
S[B
SuB
S[B
S�B
TB
TaB
T�B
U2B
UB
U�B
U�B
VB
VSB
VSB
VmB
VmB
VmB
VSB
V�B
V�B
V�B
W
B
W
B
W?B
WYB
W?B
W�B
W�B
XB
X+B
X�B
Y1B
X�B
YB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
[�B
[�B
[�B
\B
[�B
\�B
\�B
\�B
\]B
\xB
]B
\�B
]IB
]dB
]�B
^B
^B
^jB
^5B
^5B
^�B
_B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bNB
b�B
cB
cB
b�B
cB
c�B
cnB
b�B
b�B
cnB
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e,B
e,B
eFB
eFB
eFB
ezB
e�B
e�B
e�B
f2B
ffB
fLB
f�B
f�B
f�B
gB
f�B
f�B
f�B
g�B
hsB
hsB
hsB
hsB
h�B
iB
i*B
i_B
i�B
iyB
i�B
i�B
j0B
jeB
j�B
k�B
l"B
l�B
l�B
l�B
mB
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
ncB
ncB
oOB
o�B
o�B
o�B
p;B
pUB
poB
p�B
p�B
p�B
p�B
p�B
q[B
q�B
rB
rGB
r|B
r|B
r�B
r�B
r�B
sB
sMB
s�B
s�B
tB
t�B
uB
uB
uB
t�B
u%B
u%B
u%B
u%B
u�B
vB
v+B
v+B
v+B
vzB
vzB
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
|B
|B
|6B
|PB
|�B
}<B
}�B
}�B
}�B
}�B
}qB
}qB
}VB
}�B
~wB
~wB
~�B
~�B
HB
�B
�B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
��B
��B
��B
� B
��B
�B
��B
��B
�uB
��B
��B
��B
�[B
��B
��B
�-B
�GB
��B
�B
��B
��B
��B
��B
�B
�9B
��B
��B
�%B
�YB
�YB
�tB
��B
��B
�+B
�+B
��B
��B
�B
�KB
�fB
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	uB	u%B	uZB	uZB	u?B	uB	tnB	t9B	t9B	tnB	tnB	t�B	tTB	t�B	t�B	tnB	t�B	t�B	uB	t�B	t�B	t�B	tnB	u�B	��B	�DB	��B
1�B
j�B
g�B
�FBGBAoBZ�B[�BQ BB'B�B�B
�B�B�B+B@BBABH1Ba�BoiB|�B��B�zBҽB�B��B��B�B�B�B B�$B�xB��BچB�vB�VBɠB�oB�@Bv�BESB+QB 'BmB
� B
��B
�mB
��B
z�B
^�B
NB
>�B
0�B
;B
oB	�B	�B	�HB	��B	��B	�6B	�eB	HB	g�B	Y�B	PbB	E�B	72B	.�B	(�B	 vB	�B	bB	jB	�B	-B��B��B�FB��B�0B	B	1B	�B	uB	 �B	�B	�B	[B��B	B	2-B	:*B	@�B	G�B	R�B	ZB	YKB	bB	c�B	c B	a�B	c B	`�B	j�B	j0B	e�B	pB	v�B	y>B	v�B	xB	~�B	}B	��B	�;B	�B	�rB	��B	�<B	�\B	�,B	��B	�eB	��B	��B	��B	��B	�xB	�B	��B	��B	��B	�B	��B	�B	��B	�zB	��B	�+B	�B	��B	�?B	�B	�`B	��B	�<B	ĜB	�MB	�IB	�B	�TB	��B	�B	��B	�B	��B	��B	�B	޸B	ںB	�sB	�B	�KB	ܬB	��B	چB	��B	�/B	�pB	�HB	޸B	�5B	��B	�IB	�dB	�/B	ܬB	�B	�B	��B	�nB	�B	�2B	�B	�B	�B	�B	��B	� B	�B	�B	�B	�B	�8B	�ZB	��B	�B	�
B	��B	�?B	�JB	�B	�B	��B	��B	��B	�+B	��B	��B	�B	�B	��B	�AB	��B	��B	�;B	�;B	��B	��B	�JB	��B	��B	��B	��B	�`B	�B	�B	�UB	�iB	�}B	��B	�B	�|B	�%B	��B	��B	��B	�B	�LB	�B	�lB	�	B	�XB	�>B	��B	��B	�(B	�B	��B	��B	�DB	�*B	�0B	�B	�B	�wB	��B	�jB	�B	�B	��B	�*B	��B	��B	�^B	��B	�B	�jB	��B	�6B	��B	�dB	��B	�B	��B	�DB	��B	�^B	��B	��B	�0B	�0B	��B	��B	��B	�0B	�xB	��B	�B	��B	�B	��B	�JB	�JB	��B	��B	�xB	�DB	�$B	��B	�JB	�dB	��B	��B	�0B	��B	�<B	�.B
 4B
 �B
 B
 4B	��B	�]B
  B
 4B	��B	�HB	��B	�B	��B	�BB	��B	��B
 OB
 �B
B
�B
�B
�B
aB
GB
�B
gB
�B
�B
B
�B
B
B
B
EB
�B
1B
fB
�B
�B
�B
�B
�B
	�B
	lB
	lB
	�B

�B

�B

rB

�B

rB
)B
�B
JB
B
B
6B
B
6B
�B
�B
�B
6B
jB
�B
B
VB
�B
�B
�B
bB
�B
�B
B
B
hB
NB
�B
�B
�B
[B
�B
B
aB
MB
MB
�B
9B
�B
�B
YB
�B
�B
EB
EB
EB
�B
yB
�B
1B
1B
KB
�B
kB
#B
�B
]B
�B
/B
dB
IB
dB
�B
�B
�B
�B
�B
5B
OB
�B
5B
OB
�B
!B
pB
pB
 BB
 �B
!B
!bB
!|B
"NB
"�B
"�B
"�B
#TB
#nB
#�B
$@B
$tB
$tB
$�B
$�B
%FB
%,B
%`B
%zB
%�B
&B
&2B
&fB
&fB
&fB
&�B
&�B
&�B
'B
'8B
'8B
'�B
(�B
)�B
)DB
)�B
*�B
,"B
-)B
-wB
-�B
.cB
.�B
.�B
/5B
/5B
/5B
/iB
/iB
0B
/�B
0!B
0�B
0�B
0�B
1B
1AB
1AB
1[B
1�B
1�B
2-B
2GB
2|B
2�B
2�B
3�B
3�B
3�B
4�B
4�B
5%B
5ZB
5�B
5�B
6+B
6+B
6`B
6�B
6�B
6�B
8B
7�B
7�B
8�B
8�B
8�B
9$B
9>B
9XB
:DB
:DB
:B
:DB
:DB
;B
;B
;0B
;�B
<B
<B
<B
<B
<jB
<jB
<jB
<�B
<�B
<�B
<�B
<�B
=B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@ B
@4B
@�B
@�B
AB
@�B
@�B
AB
A B
AoB
BB
BB
BAB
BAB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
C�B
D�B
D�B
D�B
D�B
EB
EB
E9B
ESB
E9B
F�B
G+B
G+B
G+B
GEB
G�B
G�B
HfB
IB
I7B
IlB
IlB
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
J#B
J�B
J�B
J�B
KB
K^B
KxB
KxB
K�B
K�B
K�B
L0B
MB
MB
MB
N"B
NVB
N<B
NpB
N�B
N�B
N�B
N�B
OB
O\B
O�B
O�B
PB
P.B
PHB
P�B
Q4B
QNB
Q�B
RTB
R�B
SB
S[B
S@B
S[B
SuB
S[B
S�B
TB
TaB
T�B
U2B
UB
U�B
U�B
VB
VSB
VSB
VmB
VmB
VmB
VSB
V�B
V�B
V�B
W
B
W
B
W?B
WYB
W?B
W�B
W�B
XB
X+B
X�B
Y1B
X�B
YB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
[�B
[�B
[�B
\B
[�B
\�B
\�B
\�B
\]B
\xB
]B
\�B
]IB
]dB
]�B
^B
^B
^jB
^5B
^5B
^�B
_B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bNB
b�B
cB
cB
b�B
cB
c�B
cnB
b�B
b�B
cnB
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e,B
e,B
eFB
eFB
eFB
ezB
e�B
e�B
e�B
f2B
ffB
fLB
f�B
f�B
f�B
gB
f�B
f�B
f�B
g�B
hsB
hsB
hsB
hsB
h�B
iB
i*B
i_B
i�B
iyB
i�B
i�B
j0B
jeB
j�B
k�B
l"B
l�B
l�B
l�B
mB
mB
m)B
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
ncB
ncB
oOB
o�B
o�B
o�B
p;B
pUB
poB
p�B
p�B
p�B
p�B
p�B
q[B
q�B
rB
rGB
r|B
r|B
r�B
r�B
r�B
sB
sMB
s�B
s�B
tB
t�B
uB
uB
uB
t�B
u%B
u%B
u%B
u%B
u�B
vB
v+B
v+B
v+B
vzB
vzB
v�B
v�B
wB
wLB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
|B
|B
|6B
|PB
|�B
}<B
}�B
}�B
}�B
}�B
}qB
}qB
}VB
}�B
~wB
~wB
~�B
~�B
HB
�B
�B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
��B
��B
��B
� B
��B
�B
��B
��B
�uB
��B
��B
��B
�[B
��B
��B
�-B
�GB
��B
�B
��B
��B
��B
��B
�B
�9B
��B
��B
�%B
�YB
�YB
�tB
��B
��B
�+B
�+B
��B
��B
�B
�KB
�fB
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105249  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192850  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192851  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192851                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042858  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042858  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                