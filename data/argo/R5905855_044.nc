CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:18:23Z creation;2022-06-04T19:18:24Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191823  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�8��1   @��ʆB@.�hr�!�cz�t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B���B���B�  B�  B�  B�  Bԙ�B�33B�ffB���B�  B�  B�  B�  B�  B�ffB���B���C  C�C��C�fC
  C  C  C  C  C  C  C  C  C�C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@
>@}p�@��R@��RAA?\)A_\)A\)A�z�A��A��A��AϮA߮A�A��B�
B�
B=pB�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_p�Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B�Q�B��B��RB��B��RB��B��B��B��B��BԅB��B�Q�B߸RB��B��B��B��B��B�Q�B��B��RC��C]CC�)C	��C��C��C��C��C��C��C��C��C]C�)C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CN]CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�Dk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AҧRAҧ�AҨ�AҨ�Aҫ6Aҧ�AҪ�Aҧ�Aҩ*AҨXAҪ�A�l�A�FA�0!A�(�A�&A�"�A�!bA� \A��A�OA�	A�@A�VA�	�A�_A��.A���A�GA�\)A���Aʢ�A��A�`�A��[A��AA�~(A�ZQA�`�A�
�AƱ�A�$A�[�Aǆ�A�9XAƸRA�XEA�>BA�xlA�x�A��KA���A�K�A�b�A��LA��XA�L�A��A�3�A�A A��A��uA�[�A�k�A��wA�B[A��2A�,qA���A�U2A�.�A��8A�W
A��A�N�A���A��4Az��AuAr��ApFAn�KAlu�Aic A`�A[�AV��AS�}AN�8AJ4AH�AG�AE�OAE�AD�ADJAA�PA@��A?��A=<�A;�)A;4A9-�A7/�A4#�A2MA09�A/�A/|�A.2�A,^�A+�A)�A'�A&RTA%�A&,�A'?A%ԕA$��A"F�A .IA!�oA!ϫA e�A��A!AE9A+�A;dA:�A�PA͟A��A^�AE�A6�AA/�A�EA4�A�sAaAB[AkQA6A�A iAZ�Ak�A�A�_AJ�A��A!-A�EAn�A�A��A��AQ�A�A��Am]A��A�A�AbA
ȴA	1�AA�zAc�AA��A�QA`�A�#A�HA�At�A��A��A�A��A9XA �A��A+�A ��A �VA ��AN�A�tA��A1�A ͟A Q�@���@��Z@�-w@�%�@���@�5�@��[@�g8@���@��6@���@�O@�6z@��@��|@�Ɇ@��D@�'R@���@�@@��s@�9X@���@�>�@�_@�=�@��@��@�e@�-@�'@�#�@��}@�@�1@��@�ݘ@��@�t@�V@�H�@��@�v`@�kQ@�h@��@�w2@涮@��@���@�P@�g�@�G�@�;@�>B@��#@���@��@�Mj@��@�e,@��@޺�@�x@�.I@��@��@�
=@� i@܇�@�4@���@�6z@�	l@ڧ�@�($@ٱ[@�c�@�+@��p@�o @�l"@�@��a@�hs@ԫ6@�	@��
@Ӥ@@�\�@�ی@ҋD@�D�@�X�@���@�5?@ϸ�@�[W@·�@��@�-w@�l�@��&@ˮ�@ˤ@@�A @��@ʩ�@���@�hs@��@ȟ�@�l�@�9X@���@ǖS@�F@�5�@�8@�4�@Ƭ�@�!�@Ů�@�c�@�"�@��@�q@�4@���@�K�@��f@�E�@��3@���@�3�@��h@�f�@��@��F@�u�@��@��*@�!�@���@�a|@��@��k@�O�@��@���@���@��4@�_@���@�P�@��U@�Xy@��@���@�4�@���@�r�@�;�@���@��)@�J�@�_@��9@���@��@��@��@���@��D@�M@�{@��@�� @���@��k@�%@�ȴ@���@��1@���@��u@���@���@�ƨ@�خ@���@�m]@��@��X@�$�@��
@���@�RT@���@�H�@���@�@���@��@�b�@�/@��@���@�-�@��@�خ@��P@�F@�@���@���@���@�s�@�M�@�@�ƨ@�x�@�	l@��@��F@��@�O@�!-@�%F@���@��<@��r@� �@��-@�|@�4@��"@�ں@�_�@��@��@���@�H�@�S@��9@�S�@��.@��@�	l@��@��9@��r@�j@�@�@�@�O�@���@�ȴ@���@�`�@��@��S@�c�@�\�@��@���@�_�@�4@���@��d@�[W@��@�[�@�-�@���@��N@��Q@�hs@�!-@��s@��h@��@�l�@�L0@�0U@���@�&�@��s@��z@���@��r@�Q@�H�@�?�@�c @�D�@��@���@�P�@�0�@�@���@�w�@�6@�4@���@��w@��@��@�^�@�#�@��y@��U@���@��_@��@�{�@�Q@���@���@��S@�S&@�ی@���@�ff@�C-@�'R@��@��N@��@���@�+@���@�r�@�2�@���@��n@�4�@���@�֡@��@�D�@��r@��:@�m]@�F�@��p@���@��_@�N�@��-@���@�~�@�hs@�T�@�N<@���@�7�@��@��@�}�@�N<@�&@�ѷ@�|�@�u%@�.�@���@���@���@���@���@�.�@�@��h@�Dg@�4�@�@��8@��B@��@�u@�[@Mj@~�@~��@~ff@~�@}�C@|��@{��@{W?@zں@z� @zp;@y�3@x��@x��@x|�@xC-@w��@w~�@w.I@v��@v��@u��@uw2@uB�@t�P@t��@t�@tz�@th�@tD�@t/�@t~@s��@r�2@rh
@ru@q��@qm]@qT�@q/@p�/@p��@p~@o�@o��@on/@o8@o�@n�@n��@n�@nh
@m�.@m��@mk�@mF@m�@l�[@l�@lFt@kt�@ja|@i�3@iw2@iX@iO�@iDg@i(�@h�e@hA�@h*�@h�@g˒@g�	@gJ#@g�@f�s@f8�@d�e@c�]@c��@c��@c@O@b�\@b5?@b-@b�@a�@a��@a��@a:�@`֡@`�@`z�@`9X@_��@_g�@^s�@]�o@]�9@]��@]��@]IR@\��@\��@\q@[�}@Z�@Z�B@Z��@Y�D@Ym]@X�@Xz�@XbN@W��@WX�@V��@V��@V��@V:*@U�@U�@Tu�@TG@S��@Sn/@S33@R�@R��@RTa@R8�@R-@R($@Re@Q��@P�	@P�z@P��@P �@O��@O��@Og�@O=@O@N�@N��@N�x@NO@M�@L�I@LD�@L�@K��@K�@K��@KiD@Ke�@KK�@J��@J;�@J@I�z@I�n@H�@H��@H��@H��@H(�@G�A@Gv`@F�@F�@F��@F�@Fp;@F+k@F@E�@E!�@D�@D:�@DG@C��@C��@Ct�@B�M@B�6@BR�@A��@As�@AV@@�U@@PH@@C-@@$@?�@?J#@?�@>ں@>s�@=�@=��@=T�@=;@<"h@;��@;��@;E9@:�@:�@9��@9�X@9��@9F@9*0@9V@8�P@8�@8m�@7��@7K�@6�<@6�@5c�@5:�@5V@4�K@4��@4@3��@3��@3�@2�@2��@2Z�@2u@1�h@1j@1�@0�)@0��@0��@0�u@0_@/�W@/�0@/��@/�f@/�@.��@.�F@.YK@.$�@-��@-�@-��@-IR@-+@,Ĝ@,��@,u�@+��@+��@+RT@+&@*�B@*�@*3�@)��@)x�@)c�@)5�@)�@(�@(�@(�_@(|�@([�@(Ft@($@'�@'��@'��@']�@'
=@&�y@&�,@&�L@&�@&xl@&a|@&B[@%�D@%�@%��@%p�@%=�@$�[@$�@$9X@#�r@#�K@#U�@#6z@#&@"��@"�h@"��@"J�@"�@" �@!ԕ@!rG@!!�@ ��@ �@ ��@ �I@ (�@�0@�[@��@n/@33@��@�L@Z�@3�@{@��@Y�@�j@�o@N�@,=@7@�@��@��@�a@��@��@dZ@P�@�@��@\�@�@�d@�@hs@�@�U@j@N�@?�@$@�+@�K@�P@�@��@��@� @^5@J@�9@�C@�7@a�@<6@@�@�?@�Y@g8@I�@-�@G@��@�Q@�$@\)@4�@ i@�@�}@�@��@kQ@J@�d@��@��@o @^�@/@�@�@�@��@��@��@�@>B@�@�a@��@iD@iD@J#@C@�h@p;@H�@$�@�T@��@��@IR@��@�@��@Q�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AҧRAҧ�AҨ�AҨ�Aҫ6Aҧ�AҪ�Aҧ�Aҩ*AҨXAҪ�A�l�A�FA�0!A�(�A�&A�"�A�!bA� \A��A�OA�	A�@A�VA�	�A�_A��.A���A�GA�\)A���Aʢ�A��A�`�A��[A��AA�~(A�ZQA�`�A�
�AƱ�A�$A�[�Aǆ�A�9XAƸRA�XEA�>BA�xlA�x�A��KA���A�K�A�b�A��LA��XA�L�A��A�3�A�A A��A��uA�[�A�k�A��wA�B[A��2A�,qA���A�U2A�.�A��8A�W
A��A�N�A���A��4Az��AuAr��ApFAn�KAlu�Aic A`�A[�AV��AS�}AN�8AJ4AH�AG�AE�OAE�AD�ADJAA�PA@��A?��A=<�A;�)A;4A9-�A7/�A4#�A2MA09�A/�A/|�A.2�A,^�A+�A)�A'�A&RTA%�A&,�A'?A%ԕA$��A"F�A .IA!�oA!ϫA e�A��A!AE9A+�A;dA:�A�PA͟A��A^�AE�A6�AA/�A�EA4�A�sAaAB[AkQA6A�A iAZ�Ak�A�A�_AJ�A��A!-A�EAn�A�A��A��AQ�A�A��Am]A��A�A�AbA
ȴA	1�AA�zAc�AA��A�QA`�A�#A�HA�At�A��A��A�A��A9XA �A��A+�A ��A �VA ��AN�A�tA��A1�A ͟A Q�@���@��Z@�-w@�%�@���@�5�@��[@�g8@���@��6@���@�O@�6z@��@��|@�Ɇ@��D@�'R@���@�@@��s@�9X@���@�>�@�_@�=�@��@��@�e@�-@�'@�#�@��}@�@�1@��@�ݘ@��@�t@�V@�H�@��@�v`@�kQ@�h@��@�w2@涮@��@���@�P@�g�@�G�@�;@�>B@��#@���@��@�Mj@��@�e,@��@޺�@�x@�.I@��@��@�
=@� i@܇�@�4@���@�6z@�	l@ڧ�@�($@ٱ[@�c�@�+@��p@�o @�l"@�@��a@�hs@ԫ6@�	@��
@Ӥ@@�\�@�ی@ҋD@�D�@�X�@���@�5?@ϸ�@�[W@·�@��@�-w@�l�@��&@ˮ�@ˤ@@�A @��@ʩ�@���@�hs@��@ȟ�@�l�@�9X@���@ǖS@�F@�5�@�8@�4�@Ƭ�@�!�@Ů�@�c�@�"�@��@�q@�4@���@�K�@��f@�E�@��3@���@�3�@��h@�f�@��@��F@�u�@��@��*@�!�@���@�a|@��@��k@�O�@��@���@���@��4@�_@���@�P�@��U@�Xy@��@���@�4�@���@�r�@�;�@���@��)@�J�@�_@��9@���@��@��@��@���@��D@�M@�{@��@�� @���@��k@�%@�ȴ@���@��1@���@��u@���@���@�ƨ@�خ@���@�m]@��@��X@�$�@��
@���@�RT@���@�H�@���@�@���@��@�b�@�/@��@���@�-�@��@�خ@��P@�F@�@���@���@���@�s�@�M�@�@�ƨ@�x�@�	l@��@��F@��@�O@�!-@�%F@���@��<@��r@� �@��-@�|@�4@��"@�ں@�_�@��@��@���@�H�@�S@��9@�S�@��.@��@�	l@��@��9@��r@�j@�@�@�@�O�@���@�ȴ@���@�`�@��@��S@�c�@�\�@��@���@�_�@�4@���@��d@�[W@��@�[�@�-�@���@��N@��Q@�hs@�!-@��s@��h@��@�l�@�L0@�0U@���@�&�@��s@��z@���@��r@�Q@�H�@�?�@�c @�D�@��@���@�P�@�0�@�@���@�w�@�6@�4@���@��w@��@��@�^�@�#�@��y@��U@���@��_@��@�{�@�Q@���@���@��S@�S&@�ی@���@�ff@�C-@�'R@��@��N@��@���@�+@���@�r�@�2�@���@��n@�4�@���@�֡@��@�D�@��r@��:@�m]@�F�@��p@���@��_@�N�@��-@���@�~�@�hs@�T�@�N<@���@�7�@��@��@�}�@�N<@�&@�ѷ@�|�@�u%@�.�@���@���@���@���@���@�.�@�@��h@�Dg@�4�@�@��8@��B@��@�u@�[@Mj@~�@~��@~ff@~�@}�C@|��@{��@{W?@zں@z� @zp;@y�3@x��@x��@x|�@xC-@w��@w~�@w.I@v��@v��@u��@uw2@uB�@t�P@t��@t�@tz�@th�@tD�@t/�@t~@s��@r�2@rh
@ru@q��@qm]@qT�@q/@p�/@p��@p~@o�@o��@on/@o8@o�@n�@n��@n�@nh
@m�.@m��@mk�@mF@m�@l�[@l�@lFt@kt�@ja|@i�3@iw2@iX@iO�@iDg@i(�@h�e@hA�@h*�@h�@g˒@g�	@gJ#@g�@f�s@f8�@d�e@c�]@c��@c��@c@O@b�\@b5?@b-@b�@a�@a��@a��@a:�@`֡@`�@`z�@`9X@_��@_g�@^s�@]�o@]�9@]��@]��@]IR@\��@\��@\q@[�}@Z�@Z�B@Z��@Y�D@Ym]@X�@Xz�@XbN@W��@WX�@V��@V��@V��@V:*@U�@U�@Tu�@TG@S��@Sn/@S33@R�@R��@RTa@R8�@R-@R($@Re@Q��@P�	@P�z@P��@P �@O��@O��@Og�@O=@O@N�@N��@N�x@NO@M�@L�I@LD�@L�@K��@K�@K��@KiD@Ke�@KK�@J��@J;�@J@I�z@I�n@H�@H��@H��@H��@H(�@G�A@Gv`@F�@F�@F��@F�@Fp;@F+k@F@E�@E!�@D�@D:�@DG@C��@C��@Ct�@B�M@B�6@BR�@A��@As�@AV@@�U@@PH@@C-@@$@?�@?J#@?�@>ں@>s�@=�@=��@=T�@=;@<"h@;��@;��@;E9@:�@:�@9��@9�X@9��@9F@9*0@9V@8�P@8�@8m�@7��@7K�@6�<@6�@5c�@5:�@5V@4�K@4��@4@3��@3��@3�@2�@2��@2Z�@2u@1�h@1j@1�@0�)@0��@0��@0�u@0_@/�W@/�0@/��@/�f@/�@.��@.�F@.YK@.$�@-��@-�@-��@-IR@-+@,Ĝ@,��@,u�@+��@+��@+RT@+&@*�B@*�@*3�@)��@)x�@)c�@)5�@)�@(�@(�@(�_@(|�@([�@(Ft@($@'�@'��@'��@']�@'
=@&�y@&�,@&�L@&�@&xl@&a|@&B[@%�D@%�@%��@%p�@%=�@$�[@$�@$9X@#�r@#�K@#U�@#6z@#&@"��@"�h@"��@"J�@"�@" �@!ԕ@!rG@!!�@ ��@ �@ ��@ �I@ (�@�0@�[@��@n/@33@��@�L@Z�@3�@{@��@Y�@�j@�o@N�@,=@7@�@��@��@�a@��@��@dZ@P�@�@��@\�@�@�d@�@hs@�@�U@j@N�@?�@$@�+@�K@�P@�@��@��@� @^5@J@�9@�C@�7@a�@<6@@�@�?@�Y@g8@I�@-�@G@��@�Q@�$@\)@4�@ i@�@�}@�@��@kQ@J@�d@��@��@o @^�@/@�@�@�@��@��@��@�@>B@�@�a@��@iD@iD@J#@C@�h@p;@H�@$�@�T@��@��@IR@��@�@��@Q�@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�FB�B��B��B��B��B��B��B��B��B�[B��B� B��B��B� B�:B�TB�:B�TB�:B� B��B��B��B��B� B�"Bd�B4�B1vB-�B2B%B
B��B�cB�B%B+BBBI�Bs�B��B�B��B�B	�NB
�B
�HBB"�B
�B
��B
�(BQ�BN�B=�B#TB"�BB3B'B
ݲB
�VB
v�B
o�B
?.B
�B
�B
	�B
EB
B
fB	�B	ԯB	B	��B	~BB	wB	n}B	e�B	Z�B	NB	-�B	
B	B��B�B�BޞB�B��B�B��B�\B�B�B� B� B�;B�'B�bB�-B��B�5B�\B�B�@B�B��B	{B	�B	B	aB	�B	,WB	\)B	U�B	S[B	[=B	[	B	�[B	��B	�3B	w�B	q'B	w�B	{�B	|�B	}�B	�oB	�7B	�VB	�.B	��B	��B	��B	��B	�	B	�jB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�6B	��B	�B	�FB	��B	��B	��B	��B	�`B	��B	�"B	��B	��B	��B	��B	�jB	��B	��B	�]B	��B	�KB	��B	�6B	��B	�/B	�OB	��B	��B	�B	��B	�B	�B	�;B	��B	��B	��B	�TB	��B	��B	��B	��B	�B	ǮB	�XB	��B	��B	�@B	�{B	�YB	�eB	خB	��B	��B	��B	ۦB	�]B	ܬB	��B	��B	�B	�~B	�dB	�IB	��B	�B	ބB	��B	�B	�;B	�B	�B	��B	�B	�B	��B	�:B	�B	�B	��B	� B	�|B	�-B	��B	�B	�TB	�&B	�B	��B	��B	�`B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�:B	��B	�FB	�,B	�B	��B	�HB	�B	��B	�|B	�B	��B	��B	��B	��B	�2B	��B	�8B	��B	��B	�B	�mB	��B	�B	��B	�8B	�8B	�B	�B	�TB	�B	��B	��B	�B	��B	�tB	��B	�B	�FB	��B	�FB	�LB	��B	�B	�2B	�LB	�2B	�zB	�FB	�,B	��B	��B	��B	�RB	�B	�B	��B	��B	�B	�B	�
B	�XB	�_B	��B	�B	�B	��B	��B	�cB	�B	�]B	�B	�CB	�IB	��B	�wB	��B	��B	�B	�B	�IB	�wB	��B	�}B	�OB	�B	�B	�UB	�[B	��B	�B	�B	��B	��B	�B	�%B	�?B	��B	��B	��B	��B	�RB	�B	��B	��B	��B	�B	�RB	��B	��B	��B	�PB	��B	��B	�	B	�8B	��B	��B	�RB	��B	��B	��B	��B	��B	�(B	��B	��B	�.B	�HB	��B
 B
 �B
�B
AB
uB
 �B
;B
'B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
	B
	7B
	lB
	�B

�B
�B
dB
B
"B
VB
VB
VB
�B
�B
B
jB
B
�B
�B
�B
�B
:B
oB
�B
�B
B
�B
[B
�B
�B
�B
�B
�B
,B
uB
@B
&B
�B
:B
uB
MB
�B
�B
,B
�B
�B
NB
B
�B
TB
�B
B
uB
�B
�B
B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
2B
MB
gB
�B
�B
�B
B
�B
�B
&B
�B
B
&B
B
aB
B
9B
�B
�B
{B
�B
MB
�B
�B
�B
B
gB
gB
gB
gB
�B
�B
�B
�B
yB
�B
1B
B
B
QB
B
�B
B
�B
CB
�B
�B
CB
�B
�B
�B
/B
B
5B
�B
�B
�B
�B
5B
OB
jB
�B
�B
 'B
 �B
 �B
 �B
!�B
!�B
!�B
"4B
#nB
#:B
#nB
#nB
#�B
#�B
$�B
%�B
&LB
%�B
%�B
&LB
&fB
'B
'mB
'B
'�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
,B
,"B
,WB
,qB
,�B
-�B
-�B
.B
.}B
.�B
.�B
/ B
/B
/�B
0�B
0�B
1[B
1vB
1vB
1�B
2�B
33B
33B
3hB
3�B
49B
4nB
4�B
4�B
5%B
5tB
5�B
5�B
6zB
6�B
6�B
6�B
7B
7B
72B
7LB
7�B
7�B
7�B
7�B
8B
8B
8B
8RB
8�B
8�B
9	B
9>B
9�B
9�B
9rB
9rB
9�B
9�B
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;�B
=<B
=�B
=�B
=�B
=�B
=�B
=�B
>]B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>B
>(B
=�B
=�B
=�B
>B
>wB
>�B
?.B
?}B
@ B
@4B
@4B
?�B
?HB
?HB
?.B
?HB
?cB
@ B
@�B
@�B
B'B
BuB
B�B
B�B
B'B
BAB
CaB
C-B
CB
CB
C�B
D�B
D�B
EB
E9B
E9B
ESB
E�B
E�B
E�B
E�B
FB
F�B
GzB
HB
H�B
H�B
IB
IRB
I�B
J=B
JXB
JrB
JrB
JrB
JXB
J�B
K^B
K�B
K�B
LB
LB
L0B
L~B
L�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
N"B
NVB
N<B
N<B
NpB
N�B
N�B
NVB
N�B
O(B
O(B
OvB
O\B
O�B
O�B
PB
O�B
PbB
PbB
P�B
QB
QNB
Q4B
QB
QhB
Q�B
QhB
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
T{B
T�B
T�B
UgB
UB
U�B
U2B
VSB
VB
VB
V�B
V�B
W
B
W?B
WYB
X_B
XyB
X_B
X�B
Y1B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZQB
Z7B
ZQB
ZkB
[#B
[=B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_B
_pB
_�B
_�B
_�B
`B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
bNB
bhB
b�B
b�B
cTB
c�B
c�B
c�B
e`B
eFB
e`B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
gB
gB
gRB
gRB
gRB
g8B
gmB
g�B
g�B
hXB
h>B
h$B
h>B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
j�B
j�B
j�B
kB
kB
k�B
k�B
k�B
k�B
k�B
lB
lqB
lqB
l�B
lWB
l�B
l�B
mCB
m)B
mCB
mwB
m�B
m�B
nB
n}B
n}B
n�B
n�B
oB
o�B
o�B
p!B
p;B
pUB
pUB
pUB
pUB
p�B
p�B
p�B
p�B
p�B
qB
qvB
q�B
q�B
rGB
rGB
raB
r�B
r�B
s�B
shB
shB
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
xB
x8B
xRB
xlB
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
zB
y�B
zB
z*B
z^B
zB
zDB
z�B
z�B
z�B
{B
{�B
{JB
{�B
{�B
|B
|6B
|6B
|6B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�FB�B��B��B��B��B��B��B��B��B�[B��B� B��B��B� B�:B�TB�:B�TB�:B� B��B��B��B��B� B�"Bd�B4�B1vB-�B2B%B
B��B�cB�B%B+BBBI�Bs�B��B�B��B�B	�NB
�B
�HBB"�B
�B
��B
�(BQ�BN�B=�B#TB"�BB3B'B
ݲB
�VB
v�B
o�B
?.B
�B
�B
	�B
EB
B
fB	�B	ԯB	B	��B	~BB	wB	n}B	e�B	Z�B	NB	-�B	
B	B��B�B�BޞB�B��B�B��B�\B�B�B� B� B�;B�'B�bB�-B��B�5B�\B�B�@B�B��B	{B	�B	B	aB	�B	,WB	\)B	U�B	S[B	[=B	[	B	�[B	��B	�3B	w�B	q'B	w�B	{�B	|�B	}�B	�oB	�7B	�VB	�.B	��B	��B	��B	��B	�	B	�jB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�6B	��B	�B	�FB	��B	��B	��B	��B	�`B	��B	�"B	��B	��B	��B	��B	�jB	��B	��B	�]B	��B	�KB	��B	�6B	��B	�/B	�OB	��B	��B	�B	��B	�B	�B	�;B	��B	��B	��B	�TB	��B	��B	��B	��B	�B	ǮB	�XB	��B	��B	�@B	�{B	�YB	�eB	خB	��B	��B	��B	ۦB	�]B	ܬB	��B	��B	�B	�~B	�dB	�IB	��B	�B	ބB	��B	�B	�;B	�B	�B	��B	�B	�B	��B	�:B	�B	�B	��B	� B	�|B	�-B	��B	�B	�TB	�&B	�B	��B	��B	�`B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�:B	��B	�FB	�,B	�B	��B	�HB	�B	��B	�|B	�B	��B	��B	��B	��B	�2B	��B	�8B	��B	��B	�B	�mB	��B	�B	��B	�8B	�8B	�B	�B	�TB	�B	��B	��B	�B	��B	�tB	��B	�B	�FB	��B	�FB	�LB	��B	�B	�2B	�LB	�2B	�zB	�FB	�,B	��B	��B	��B	�RB	�B	�B	��B	��B	�B	�B	�
B	�XB	�_B	��B	�B	�B	��B	��B	�cB	�B	�]B	�B	�CB	�IB	��B	�wB	��B	��B	�B	�B	�IB	�wB	��B	�}B	�OB	�B	�B	�UB	�[B	��B	�B	�B	��B	��B	�B	�%B	�?B	��B	��B	��B	��B	�RB	�B	��B	��B	��B	�B	�RB	��B	��B	��B	�PB	��B	��B	�	B	�8B	��B	��B	�RB	��B	��B	��B	��B	��B	�(B	��B	��B	�.B	�HB	��B
 B
 �B
�B
AB
uB
 �B
;B
'B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
	B
	7B
	lB
	�B

�B
�B
dB
B
"B
VB
VB
VB
�B
�B
B
jB
B
�B
�B
�B
�B
:B
oB
�B
�B
B
�B
[B
�B
�B
�B
�B
�B
,B
uB
@B
&B
�B
:B
uB
MB
�B
�B
,B
�B
�B
NB
B
�B
TB
�B
B
uB
�B
�B
B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
2B
MB
gB
�B
�B
�B
B
�B
�B
&B
�B
B
&B
B
aB
B
9B
�B
�B
{B
�B
MB
�B
�B
�B
B
gB
gB
gB
gB
�B
�B
�B
�B
yB
�B
1B
B
B
QB
B
�B
B
�B
CB
�B
�B
CB
�B
�B
�B
/B
B
5B
�B
�B
�B
�B
5B
OB
jB
�B
�B
 'B
 �B
 �B
 �B
!�B
!�B
!�B
"4B
#nB
#:B
#nB
#nB
#�B
#�B
$�B
%�B
&LB
%�B
%�B
&LB
&fB
'B
'mB
'B
'�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
,B
,"B
,WB
,qB
,�B
-�B
-�B
.B
.}B
.�B
.�B
/ B
/B
/�B
0�B
0�B
1[B
1vB
1vB
1�B
2�B
33B
33B
3hB
3�B
49B
4nB
4�B
4�B
5%B
5tB
5�B
5�B
6zB
6�B
6�B
6�B
7B
7B
72B
7LB
7�B
7�B
7�B
7�B
8B
8B
8B
8RB
8�B
8�B
9	B
9>B
9�B
9�B
9rB
9rB
9�B
9�B
9�B
9�B
9�B
:DB
:xB
:�B
:�B
;B
;0B
;�B
=<B
=�B
=�B
=�B
=�B
=�B
=�B
>]B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>B
>(B
=�B
=�B
=�B
>B
>wB
>�B
?.B
?}B
@ B
@4B
@4B
?�B
?HB
?HB
?.B
?HB
?cB
@ B
@�B
@�B
B'B
BuB
B�B
B�B
B'B
BAB
CaB
C-B
CB
CB
C�B
D�B
D�B
EB
E9B
E9B
ESB
E�B
E�B
E�B
E�B
FB
F�B
GzB
HB
H�B
H�B
IB
IRB
I�B
J=B
JXB
JrB
JrB
JrB
JXB
J�B
K^B
K�B
K�B
LB
LB
L0B
L~B
L�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
N"B
NVB
N<B
N<B
NpB
N�B
N�B
NVB
N�B
O(B
O(B
OvB
O\B
O�B
O�B
PB
O�B
PbB
PbB
P�B
QB
QNB
Q4B
QB
QhB
Q�B
QhB
Q�B
R:B
RoB
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
T{B
T�B
T�B
UgB
UB
U�B
U2B
VSB
VB
VB
V�B
V�B
W
B
W?B
WYB
X_B
XyB
X_B
X�B
Y1B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZQB
Z7B
ZQB
ZkB
[#B
[=B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_B
_pB
_�B
_�B
_�B
`B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
a-B
abB
abB
a�B
a�B
a�B
a�B
a�B
bNB
bhB
b�B
b�B
cTB
c�B
c�B
c�B
e`B
eFB
e`B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
gB
gB
gRB
gRB
gRB
g8B
gmB
g�B
g�B
hXB
h>B
h$B
h>B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
j�B
j�B
j�B
kB
kB
k�B
k�B
k�B
k�B
k�B
lB
lqB
lqB
l�B
lWB
l�B
l�B
mCB
m)B
mCB
mwB
m�B
m�B
nB
n}B
n}B
n�B
n�B
oB
o�B
o�B
p!B
p;B
pUB
pUB
pUB
pUB
p�B
p�B
p�B
p�B
p�B
qB
qvB
q�B
q�B
rGB
rGB
raB
r�B
r�B
s�B
shB
shB
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wB
wB
wfB
w�B
w�B
w�B
xB
x8B
xRB
xlB
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
zB
y�B
zB
z*B
z^B
zB
zDB
z�B
z�B
z�B
{B
{�B
{JB
{�B
{�B
|B
|6B
|6B
|6B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105236  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191823  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191824  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191824                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041831  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041831  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                