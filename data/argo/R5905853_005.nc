CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:21Z creation;2022-06-04T17:23:22Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172321  20220610121506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ذ�Tb�1   @ذֻ*@,�-V�dp�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A��A   AA��A[33A~ffA�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH��BP��BV  B_��Bg��Bp  Bx  B��B�  B�  B���B���B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33Bߙ�B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C833C:  C<  C>  C@�CA�fCC�fCF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� Dz��D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @
>@��@��RA ��A\)A@��AZ�\A}A��A��A��GA��AϮA߮A�A��B�
B�
B�
B�
B'p�B/�
B7�
B?�
BH��BP��BU�
B_p�Bgp�Bo�
Bw�
Bp�B��B��B��RB��RB��B��B��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B�Q�B��B߅B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C]C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)�)C+��C-��C/��C1��C3��C5��C8(�C9��C;��C=��C@]CA�)CC�)CE��CG�)CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD,�D,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�D:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�D{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�>�A�CaA�K^A�QA�_pA�[WA�gmA�m]A�oiA�o�A�r�A�o�A�l�A�m�A�p�A�sA�sA�u%A�p�A�u�A�t�A�qvA�v�A�a�A��A�YAΊ=A�
�A��DAˡbA�MAʀiAʽ�A�~]Aɻ0A��AA��]A�OvA�-�A��A��A�|�A�)�A�_�A�PA��<A��A�;dA���A���A��wA�|A�v`A��FA��JA��_A�>BA��?A�� A�r�A�R�A���A�GEA��A�xA��uA�oA��A�(A�0�A�R�A�A�S�A��^A��2A��A�$�A���A�]/A���A��mA�0!A~�UAydZAs>�Ap�ApeAn��Ai��Ag�Ad
=Ac \Ab�AbTaAaԕA`:�AXB[AQ�pAOg8AM�CAL	�AJ�kAI�:AJI�AI��AG��AC�A@Q�A?�hA?��A?0UA=.�A<xA;2�A:l�A9S�A7��A4��A2��A0�mA.^5A-��A-tTA-/A-�A-�A,�A,c�A+��A)]�A(A�A'��A'�0A'?A%�A%�_A%Z�A%J�A$�]A$A$/�A%6�A%/�A$��A"�'A!_A!��A!�{A #:A��AA��A��A��AK^A iA��A^�A��A��Au%A_pAdZAp�Av�As�A \A�AIRA��A�AA�]A��A�PAMjA
�\A
IRA	֡A	��A	�A�sA� Ap;A*�A�A�ASAߤA��A�"A�rA��AYKA.IA��A�rAVA��A�HA��A�oAVA�A��A[WAA��AI�A0�A ��A �3A ��A ]�@�Vm@��@���@�Vm@�	l@�(�@�G�@�d�@��@�|�@���@�S@���@���@���@��@�I�@�#�@�b@�@�@���@�u%@��@�(�@흲@�z�@��@��@��@�8�@軙@� �@�
=@��@�P�@��@��@�  @��@�_@���@���@�N�@�?}@��@ݚk@݁�@�m]@�G�@��@�2�@�˒@ۋ�@�O@��s@�;�@ٿH@�F@ؑ�@�@�{J@��@��@ֹ�@�ff@�ԕ@�}V@�-�@�u@���@Ӂ�@�	l@�{�@�e@ѿH@�j�@��'@�s�@��Q@ϭC@ώ�@�\�@γh@�|�@�Xy@�	�@���@͎�@�;d@�4@˘�@�O�@�@@��@��5@��@�;�@ɹ�@�?}@Ⱥ�@�g8@��@�g�@�S�@�$t@ƺ�@ƙ1@�E�@Ŏ"@�+@�z�@�~@���@×$@�5�@�@�R�@���@��@��_@�h
@�W�@�Ft@�<�@�*�@��Z@���@��{@�S�@�Mj@��@�"h@���@���@���@�w2@�J�@�	l@���@�?�@�	@�u@��A@��@�]�@���@��@�Q@��W@�U�@��@�u%@�[�@�@�@���@��@�ں@�c @��@��7@���@���@�M�@�)�@��.@���@�A�@���@�ff@�Q@��@�~�@�ߤ@�M@���@�C�@��8@��@�N�@��@���@���@��@�X�@�*0@�@@�;@��p@�a|@�9X@�O@��@�iD@��@��\@��@�u�@�Vm@���@�M@��@���@���@��<@�a|@���@��]@�9X@���@��H@���@�u�@�_@�L0@�	@���@��f@�o @� \@�R�@���@��@��<@��j@��j@�E�@��
@�|�@�dZ@�O�@�33@��M@���@��@��@�rG@�Mj@�.I@��@��M@��)@��R@��\@�xl@�Q@�'R@��z@�v`@��h@���@��@�u�@�6�@�	�@���@���@���@�p�@��@��@���@�}V@�kQ@��@�+@��z@�~@���@�rG@�&�@���@���@���@���@�L0@�.�@��@�@�� @�/�@��@��@�J�@�3�@��@�y�@�;d@�!-@��@��y@��p@���@�ff@�/�@��@��@��o@��
@��K@��X@���@�Vm@�(�@���@��L@�oi@�+k@���@���@�f�@��@���@���@���@�r�@��]@��@��P@�|�@�Vm@�!�@�ѷ@���@�@���@�o @�4@��@��@��@�}V@�S�@���@���@�[W@�*0@�Ĝ@�U2@���@�Vm@�Dg@��@��X@�q�@�0U@��@��@�RT@�9�@�V@���@�͟@���@��_@�q�@�1�@�;@��@o@~�M@~��@~�F@~\�@~C�@~0U@~	@}�@}��@}/@|�@|ی@|��@|U2@{��@{��@zC�@y��@x�@x~(@x1'@wX�@v��@v��@v:*@u��@u��@t�v@tw�@t2�@t	�@s�P@s�@s i@r�]@r��@rL0@q��@q��@q+@poi@o��@o@O@n\�@m��@m�@l�@l��@k��@kJ#@k8@kC@j��@j6�@i��@i�@h*�@g��@g'�@gS@f�1@fJ@eY�@d�@d��@dc�@dH@c�@b�@b_�@a�S@`֡@`bN@`�@_C�@^��@^+k@]m]@\��@[�@[�@[4�@Z�1@Y�D@Y�S@Y[W@Y�@X�9@W��@WZ�@WH�@W�@V�@VM�@U�>@U�@U�@T�@T�@T��@Tj@S�	@R�"@Ra|@Q��@Q��@P��@Pe�@Pb@O��@OiD@O8@N�y@N{�@M��@MO�@M=�@L�z@L'R@K�@K��@KC�@K�@J}V@Ju@I�"@Ip�@I+@H`�@G��@G�f@GRT@F��@F�2@F�6@F_�@F	@Eϫ@E�~@Ef�@D�f@D�/@D��@D�O@DU2@C�W@C��@C�F@CZ�@C�@B��@B��@B��@Bd�@B	@A�@Aϫ@A�n@Af�@A�@@�@@�u@@�@?_p@?$t@>�h@>=q@>�@=�d@=-w@<��@<�v@<��@<��@<[�@<N�@<G@;��@;6z@:�!@:M�@:J@9��@9;@8h�@7��@7@O@6��@6�6@6kQ@5�z@5(�@4�`@4:�@3ݘ@3,�@2�M@2��@2l�@2_�@2;�@2u@1�@1�@1�@0��@0w�@0g8@0]d@0'R@/��@/1�@.�H@.~�@.Ov@.#:@. �@-�=@-f�@-*0@,�@,�E@,�9@,��@,M@,�@+�K@+�{@++@*��@*��@*��@*{�@*M�@*C�@*:*@*.�@)�@)N<@)+�@)@@(�v@(w�@(�@'�K@'~�@'a@'�@&�M@&�R@&B[@&�@%�@%�H@%�=@%hs@%+@$��@$�K@$Ĝ@$��@$S�@#��@#��@#�@#X�@#H�@#1�@#�@"͟@"�@"C�@"�@!�@!�d@!��@!2a@ �@ �4@ �Y@ V�@ <�@ <�@ 	�@j�@�y@�L@n�@�>@�-@��@+�@��@�I@g8@7@�@��@�{@4�@�@�@��@�6@�\@l�@&�@�@��@^�@��@��@w�@q@e�@Ft@:�@%�@��@�@]�@/�@�@��@�M@�@�A@GE@��@�#@��@Y�@��@�@��@�$@��@�@m�@7@@b@��@~�@&@��@�@ȴ@�!@�b@}V@B[@�)@��@�@��@|@:�@��@�[@��@��@��@�o@m�@c�@-�@�@��@��@a@Y@��@�H@�@��@�@�L@�@��@��@��@c @&�@-@{@ϫ@��@�C@�h@7L@+@V@�/@�$@��@r�@I�@!@�r@�@�]@�]@�]@�r@�r@�@��@P�@,�@
�8@
��@
��@
c @
�@	�T@	�N@	��@	|@	^�@	Dg11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�>�A�CaA�K^A�QA�_pA�[WA�gmA�m]A�oiA�o�A�r�A�o�A�l�A�m�A�p�A�sA�sA�u%A�p�A�u�A�t�A�qvA�v�A�a�A��A�YAΊ=A�
�A��DAˡbA�MAʀiAʽ�A�~]Aɻ0A��AA��]A�OvA�-�A��A��A�|�A�)�A�_�A�PA��<A��A�;dA���A���A��wA�|A�v`A��FA��JA��_A�>BA��?A�� A�r�A�R�A���A�GEA��A�xA��uA�oA��A�(A�0�A�R�A�A�S�A��^A��2A��A�$�A���A�]/A���A��mA�0!A~�UAydZAs>�Ap�ApeAn��Ai��Ag�Ad
=Ac \Ab�AbTaAaԕA`:�AXB[AQ�pAOg8AM�CAL	�AJ�kAI�:AJI�AI��AG��AC�A@Q�A?�hA?��A?0UA=.�A<xA;2�A:l�A9S�A7��A4��A2��A0�mA.^5A-��A-tTA-/A-�A-�A,�A,c�A+��A)]�A(A�A'��A'�0A'?A%�A%�_A%Z�A%J�A$�]A$A$/�A%6�A%/�A$��A"�'A!_A!��A!�{A #:A��AA��A��A��AK^A iA��A^�A��A��Au%A_pAdZAp�Av�As�A \A�AIRA��A�AA�]A��A�PAMjA
�\A
IRA	֡A	��A	�A�sA� Ap;A*�A�A�ASAߤA��A�"A�rA��AYKA.IA��A�rAVA��A�HA��A�oAVA�A��A[WAA��AI�A0�A ��A �3A ��A ]�@�Vm@��@���@�Vm@�	l@�(�@�G�@�d�@��@�|�@���@�S@���@���@���@��@�I�@�#�@�b@�@�@���@�u%@��@�(�@흲@�z�@��@��@��@�8�@軙@� �@�
=@��@�P�@��@��@�  @��@�_@���@���@�N�@�?}@��@ݚk@݁�@�m]@�G�@��@�2�@�˒@ۋ�@�O@��s@�;�@ٿH@�F@ؑ�@�@�{J@��@��@ֹ�@�ff@�ԕ@�}V@�-�@�u@���@Ӂ�@�	l@�{�@�e@ѿH@�j�@��'@�s�@��Q@ϭC@ώ�@�\�@γh@�|�@�Xy@�	�@���@͎�@�;d@�4@˘�@�O�@�@@��@��5@��@�;�@ɹ�@�?}@Ⱥ�@�g8@��@�g�@�S�@�$t@ƺ�@ƙ1@�E�@Ŏ"@�+@�z�@�~@���@×$@�5�@�@�R�@���@��@��_@�h
@�W�@�Ft@�<�@�*�@��Z@���@��{@�S�@�Mj@��@�"h@���@���@���@�w2@�J�@�	l@���@�?�@�	@�u@��A@��@�]�@���@��@�Q@��W@�U�@��@�u%@�[�@�@�@���@��@�ں@�c @��@��7@���@���@�M�@�)�@��.@���@�A�@���@�ff@�Q@��@�~�@�ߤ@�M@���@�C�@��8@��@�N�@��@���@���@��@�X�@�*0@�@@�;@��p@�a|@�9X@�O@��@�iD@��@��\@��@�u�@�Vm@���@�M@��@���@���@��<@�a|@���@��]@�9X@���@��H@���@�u�@�_@�L0@�	@���@��f@�o @� \@�R�@���@��@��<@��j@��j@�E�@��
@�|�@�dZ@�O�@�33@��M@���@��@��@�rG@�Mj@�.I@��@��M@��)@��R@��\@�xl@�Q@�'R@��z@�v`@��h@���@��@�u�@�6�@�	�@���@���@���@�p�@��@��@���@�}V@�kQ@��@�+@��z@�~@���@�rG@�&�@���@���@���@���@�L0@�.�@��@�@�� @�/�@��@��@�J�@�3�@��@�y�@�;d@�!-@��@��y@��p@���@�ff@�/�@��@��@��o@��
@��K@��X@���@�Vm@�(�@���@��L@�oi@�+k@���@���@�f�@��@���@���@���@�r�@��]@��@��P@�|�@�Vm@�!�@�ѷ@���@�@���@�o @�4@��@��@��@�}V@�S�@���@���@�[W@�*0@�Ĝ@�U2@���@�Vm@�Dg@��@��X@�q�@�0U@��@��@�RT@�9�@�V@���@�͟@���@��_@�q�@�1�@�;@��@o@~�M@~��@~�F@~\�@~C�@~0U@~	@}�@}��@}/@|�@|ی@|��@|U2@{��@{��@zC�@y��@x�@x~(@x1'@wX�@v��@v��@v:*@u��@u��@t�v@tw�@t2�@t	�@s�P@s�@s i@r�]@r��@rL0@q��@q��@q+@poi@o��@o@O@n\�@m��@m�@l�@l��@k��@kJ#@k8@kC@j��@j6�@i��@i�@h*�@g��@g'�@gS@f�1@fJ@eY�@d�@d��@dc�@dH@c�@b�@b_�@a�S@`֡@`bN@`�@_C�@^��@^+k@]m]@\��@[�@[�@[4�@Z�1@Y�D@Y�S@Y[W@Y�@X�9@W��@WZ�@WH�@W�@V�@VM�@U�>@U�@U�@T�@T�@T��@Tj@S�	@R�"@Ra|@Q��@Q��@P��@Pe�@Pb@O��@OiD@O8@N�y@N{�@M��@MO�@M=�@L�z@L'R@K�@K��@KC�@K�@J}V@Ju@I�"@Ip�@I+@H`�@G��@G�f@GRT@F��@F�2@F�6@F_�@F	@Eϫ@E�~@Ef�@D�f@D�/@D��@D�O@DU2@C�W@C��@C�F@CZ�@C�@B��@B��@B��@Bd�@B	@A�@Aϫ@A�n@Af�@A�@@�@@�u@@�@?_p@?$t@>�h@>=q@>�@=�d@=-w@<��@<�v@<��@<��@<[�@<N�@<G@;��@;6z@:�!@:M�@:J@9��@9;@8h�@7��@7@O@6��@6�6@6kQ@5�z@5(�@4�`@4:�@3ݘ@3,�@2�M@2��@2l�@2_�@2;�@2u@1�@1�@1�@0��@0w�@0g8@0]d@0'R@/��@/1�@.�H@.~�@.Ov@.#:@. �@-�=@-f�@-*0@,�@,�E@,�9@,��@,M@,�@+�K@+�{@++@*��@*��@*��@*{�@*M�@*C�@*:*@*.�@)�@)N<@)+�@)@@(�v@(w�@(�@'�K@'~�@'a@'�@&�M@&�R@&B[@&�@%�@%�H@%�=@%hs@%+@$��@$�K@$Ĝ@$��@$S�@#��@#��@#�@#X�@#H�@#1�@#�@"͟@"�@"C�@"�@!�@!�d@!��@!2a@ �@ �4@ �Y@ V�@ <�@ <�@ 	�@j�@�y@�L@n�@�>@�-@��@+�@��@�I@g8@7@�@��@�{@4�@�@�@��@�6@�\@l�@&�@�@��@^�@��@��@w�@q@e�@Ft@:�@%�@��@�@]�@/�@�@��@�M@�@�A@GE@��@�#@��@Y�@��@�@��@�$@��@�@m�@7@@b@��@~�@&@��@�@ȴ@�!@�b@}V@B[@�)@��@�@��@|@:�@��@�[@��@��@��@�o@m�@c�@-�@�@��@��@a@Y@��@�H@�@��@�@�L@�@��@��@��@c @&�@-@{@ϫ@��@�C@�h@7L@+@V@�/@�$@��@r�@I�@!@�r@�@�]@�]@�]@�r@�r@�@��@P�@,�@
�8@
��@
��@
c @
�@	�T@	�N@	��@	|@	^�@	Dg11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BqB�B�B�B�B=B�B=B=B�B�BqB	B#B	B	BWB�B=B=B#B=B#BxB)DBNVBUMB}�B��B	,�B	1[B	J	B	�B
C�B
�;BsBZ�B��B��B�=B�;B��B�B��B�B�BGB�BdB<B�B*�B7LB5�B4�B5�B4nB(�B B
B�B	RB)B��BߊB��B��B�lBkBQNB5�BB
�JB
�/B
�B
�mB
��B
�B
R:B
,B
?B
 �B	�B	�-B	��B	��B	��B	�]B	��B	�B	p;B	^5B	VSB	R:B	P�B	M6B	B�B	�B	�B��B�B�B�;B�B	/B	B�B	C-B	(�B	*�B	0�B	6�B	O�B	WYB	V�B	R�B	P}B	P�B	L�B	;�B	0�B	!HB	 �B	&�B	*�B	/ B	2�B	3�B	:�B	J�B	X�B	nIB	q�B	s3B	r�B	tB	�B	�B	��B	��B	�CB	�B	�GB	�HB	��B	�CB	��B	�#B	�tB	��B	��B	��B	�/B	��B	��B	�qB	�VB	��B	��B	�B	��B	��B	�B	�xB	�^B	��B	��B	��B	�`B	�xB	�EB	�sB	ӏB	� B	�@B	��B	ңB	̘B	ʌB	�fB	�B	�7B	�7B	ȴB	�0B	̈́B	̈́B	�BB	��B	�B	�B	ևB	�_B	�qB	�/B	�B	�QB	�!B	�bB	�nB	�B	��B	�B	�B	�hB	�B	ޞB	ߊB	�]B	�7B	��B	ٚB	ٴB	�B	�B	��B	��B	��B	�OB	�B	�!B	��B	��B	��B	�B	��B	�BB	�B	�HB	�B	�B	�B	��B	�B	��B	�B	�0B	��B	�eB	�>B	�B	�8B	��B	�UB	�iB	�B	�B	��B	�@B	�B	�B	�nB	�B	�ZB	�B	�B	�B	�B	�$B	�$B	�*B	�_B	�B	��B	��B	�B	�KB	��B	�B	�B	�B	�QB	�B	�B	�QB	�B	�"B	�wB	�B	��B	�B	�B	��B	��B	�B	�MB	�hB	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�2B	�B	��B	�B	�B	��B	�B	��B	�TB	�TB	�TB	�%B	�nB	�B	�9B	��B	�tB	��B	�9B	��B	�`B	��B	�tB	��B	�9B	�B	�B	��B	�TB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�2B	�2B	�LB	��B	�LB	�fB	�2B	�B	�B	�B	�8B	��B	��B	�dB	�B	�B	�dB	�B	�B	�6B	�B	�B	�PB	��B	�"B	�B	��B	�qB	��B	�B	�HB	��B	��B
 �B
;B
�B
�B
�B
�B
�B
[B
�B
uB
'B
AB
uB
aB
�B
gB
�B
�B
B
SB
�B
�B
�B
�B
�B
B
�B
�B
%B
tB
�B
�B
�B
zB
+B
�B
	�B

XB

=B

�B
dB
dB
~B
�B
�B
�B
VB
�B
�B
B
HB
�B
hB
�B
B
�B
{B
�B
aB
�B
�B
�B
�B
�B
�B
B

B
�B

B
�B
�B
�B
�B
�B
�B
�B
EB
eB
B
�B
�B
�B
�B
B
7B
QB
QB
�B
QB
QB
B
�B
QB
QB
kB
�B
�B
�B
�B
#B
WB
�B
�B
WB
xB
�B
�B
�B
�B
�B
B
pB
�B
�B
!�B
#B
#B
# B
#B
#�B
$�B
$�B
%�B
%�B
%�B
&LB
&�B
'B
&�B
'B
'RB
'8B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(XB
(sB
(sB
(�B
(�B
(�B
)*B
)_B
)yB
)�B
*B
*KB
*eB
*eB
*�B
*�B
+6B
+QB
+�B
+�B
,"B
,=B
-]B
-�B
.cB
.cB
.cB
.cB
.�B
.�B
/ B
/�B
0B
0UB
0oB
1'B
1vB
2B
2�B
2�B
2�B
33B
3�B
4B
4TB
4�B
5%B
5ZB
5ZB
5?B
5�B
5tB
5�B
5�B
5�B
5ZB
5�B
6zB
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8lB
8�B
8�B
8�B
8�B
9�B
9�B
:DB
:xB
:xB
;0B
;0B
;B
;�B
;�B
;�B
<jB
<jB
<�B
<�B
="B
=VB
=<B
=VB
=�B
=qB
=�B
=�B
=�B
>]B
>�B
>�B
?.B
?cB
?�B
?�B
@B
@�B
@�B
@�B
@�B
A B
A B
AoB
A�B
BAB
B�B
CB
B�B
CGB
C�B
C�B
DgB
DgB
D�B
D�B
D�B
EB
EmB
E�B
FtB
F�B
F�B
F�B
GB
G�B
H1B
H�B
H�B
H�B
I�B
I�B
J=B
J=B
J=B
J=B
JXB
JXB
JXB
JXB
J=B
J�B
KB
K^B
K^B
K�B
K�B
K�B
K�B
K�B
L~B
MB
M�B
M�B
M�B
N�B
N�B
O(B
O(B
OvB
OvB
O�B
O�B
PbB
P�B
P�B
Q�B
Q�B
RB
R B
R:B
RTB
R�B
R�B
SB
SB
S&B
S�B
T,B
TFB
TFB
TaB
TaB
T�B
T�B
U2B
U�B
U�B
U�B
V9B
VB
VB
VB
V�B
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
XEB
XEB
X_B
X_B
X�B
X�B
YB
X�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[	B
[qB
[qB
[WB
[qB
[�B
[�B
[�B
\B
\]B
\�B
]B
]~B
]~B
]~B
]�B
]�B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`B
`�B
a-B
a�B
a�B
a�B
bB
a�B
bB
b4B
bNB
b�B
b�B
bhB
b�B
b�B
b�B
cB
c�B
c�B
d&B
c�B
c�B
dB
d&B
d�B
eB
eB
eFB
ezB
e`B
eFB
e�B
e�B
f2B
f2B
e�B
e�B
fB
f�B
g8B
gB
gB
gB
gB
gRB
gmB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
hXB
hsB
h�B
i�B
j0B
i�B
i�B
jKB
j�B
j�B
kQB
kkB
k�B
k�B
k�B
k�B
lB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mCB
mCB
mCB
mCB
m�B
n/B
n�B
n�B
o B
oOB
oOB
oOB
oOB
oiB
oOB
o�B
o�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
rGB
r|B
raB
r|B
r�B
r�B
s3B
s3B
sMB
shB
s�B
s�B
tB
tTB
tTB
t�B
t�B
u�B
u�B
vB
vFB
v+B
v+B
v`B
vzB
v`B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
xB
x�B
x�B
x�B
x�B
y	B
yrB
y�B
yrB
y�B
yrB
yXB
y�B
z�B
z�B
{0B
{JB
{JB
{JB
{JB
{�B
{�B
|PB
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
~BB
~]B
~]B
~BB
~(B
~BB
~(B
~(B
~(B
~(B
~(B
~wB
~�B
.B
.B
~�B
~�B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�4B
��B
��B
�B
�;B
�UB
�oB
�oB
�oB
�UB
��B
�AB
�AB
�uB
��B
��B
��B
��B
�B
��B
�B
�aB
�aB
�G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BqB�B�B�B�B=B�B=B=B�B�BqB	B#B	B	BWB�B=B=B#B=B#BxB)DBNVBUMB}�B��B	,�B	1[B	J	B	�B
C�B
�;BsBZ�B��B��B�=B�;B��B�B��B�B�BGB�BdB<B�B*�B7LB5�B4�B5�B4nB(�B B
B�B	RB)B��BߊB��B��B�lBkBQNB5�BB
�JB
�/B
�B
�mB
��B
�B
R:B
,B
?B
 �B	�B	�-B	��B	��B	��B	�]B	��B	�B	p;B	^5B	VSB	R:B	P�B	M6B	B�B	�B	�B��B�B�B�;B�B	/B	B�B	C-B	(�B	*�B	0�B	6�B	O�B	WYB	V�B	R�B	P}B	P�B	L�B	;�B	0�B	!HB	 �B	&�B	*�B	/ B	2�B	3�B	:�B	J�B	X�B	nIB	q�B	s3B	r�B	tB	�B	�B	��B	��B	�CB	�B	�GB	�HB	��B	�CB	��B	�#B	�tB	��B	��B	��B	�/B	��B	��B	�qB	�VB	��B	��B	�B	��B	��B	�B	�xB	�^B	��B	��B	��B	�`B	�xB	�EB	�sB	ӏB	� B	�@B	��B	ңB	̘B	ʌB	�fB	�B	�7B	�7B	ȴB	�0B	̈́B	̈́B	�BB	��B	�B	�B	ևB	�_B	�qB	�/B	�B	�QB	�!B	�bB	�nB	�B	��B	�B	�B	�hB	�B	ޞB	ߊB	�]B	�7B	��B	ٚB	ٴB	�B	�B	��B	��B	��B	�OB	�B	�!B	��B	��B	��B	�B	��B	�BB	�B	�HB	�B	�B	�B	��B	�B	��B	�B	�0B	��B	�eB	�>B	�B	�8B	��B	�UB	�iB	�B	�B	��B	�@B	�B	�B	�nB	�B	�ZB	�B	�B	�B	�B	�$B	�$B	�*B	�_B	�B	��B	��B	�B	�KB	��B	�B	�B	�B	�QB	�B	�B	�QB	�B	�"B	�wB	�B	��B	�B	�B	��B	��B	�B	�MB	�hB	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�2B	�B	��B	�B	�B	��B	�B	��B	�TB	�TB	�TB	�%B	�nB	�B	�9B	��B	�tB	��B	�9B	��B	�`B	��B	�tB	��B	�9B	�B	�B	��B	�TB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�LB	�2B	�2B	�LB	��B	�LB	�fB	�2B	�B	�B	�B	�8B	��B	��B	�dB	�B	�B	�dB	�B	�B	�6B	�B	�B	�PB	��B	�"B	�B	��B	�qB	��B	�B	�HB	��B	��B
 �B
;B
�B
�B
�B
�B
�B
[B
�B
uB
'B
AB
uB
aB
�B
gB
�B
�B
B
SB
�B
�B
�B
�B
�B
B
�B
�B
%B
tB
�B
�B
�B
zB
+B
�B
	�B

XB

=B

�B
dB
dB
~B
�B
�B
�B
VB
�B
�B
B
HB
�B
hB
�B
B
�B
{B
�B
aB
�B
�B
�B
�B
�B
�B
B

B
�B

B
�B
�B
�B
�B
�B
�B
�B
EB
eB
B
�B
�B
�B
�B
B
7B
QB
QB
�B
QB
QB
B
�B
QB
QB
kB
�B
�B
�B
�B
#B
WB
�B
�B
WB
xB
�B
�B
�B
�B
�B
B
pB
�B
�B
!�B
#B
#B
# B
#B
#�B
$�B
$�B
%�B
%�B
%�B
&LB
&�B
'B
&�B
'B
'RB
'8B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(XB
(sB
(sB
(�B
(�B
(�B
)*B
)_B
)yB
)�B
*B
*KB
*eB
*eB
*�B
*�B
+6B
+QB
+�B
+�B
,"B
,=B
-]B
-�B
.cB
.cB
.cB
.cB
.�B
.�B
/ B
/�B
0B
0UB
0oB
1'B
1vB
2B
2�B
2�B
2�B
33B
3�B
4B
4TB
4�B
5%B
5ZB
5ZB
5?B
5�B
5tB
5�B
5�B
5�B
5ZB
5�B
6zB
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
88B
8lB
8�B
8�B
8�B
8�B
9�B
9�B
:DB
:xB
:xB
;0B
;0B
;B
;�B
;�B
;�B
<jB
<jB
<�B
<�B
="B
=VB
=<B
=VB
=�B
=qB
=�B
=�B
=�B
>]B
>�B
>�B
?.B
?cB
?�B
?�B
@B
@�B
@�B
@�B
@�B
A B
A B
AoB
A�B
BAB
B�B
CB
B�B
CGB
C�B
C�B
DgB
DgB
D�B
D�B
D�B
EB
EmB
E�B
FtB
F�B
F�B
F�B
GB
G�B
H1B
H�B
H�B
H�B
I�B
I�B
J=B
J=B
J=B
J=B
JXB
JXB
JXB
JXB
J=B
J�B
KB
K^B
K^B
K�B
K�B
K�B
K�B
K�B
L~B
MB
M�B
M�B
M�B
N�B
N�B
O(B
O(B
OvB
OvB
O�B
O�B
PbB
P�B
P�B
Q�B
Q�B
RB
R B
R:B
RTB
R�B
R�B
SB
SB
S&B
S�B
T,B
TFB
TFB
TaB
TaB
T�B
T�B
U2B
U�B
U�B
U�B
V9B
VB
VB
VB
V�B
V�B
V�B
V�B
W?B
WsB
W�B
W�B
W�B
W�B
XEB
XEB
X_B
X_B
X�B
X�B
YB
X�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[	B
[qB
[qB
[WB
[qB
[�B
[�B
[�B
\B
\]B
\�B
]B
]~B
]~B
]~B
]�B
]�B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`B
`�B
a-B
a�B
a�B
a�B
bB
a�B
bB
b4B
bNB
b�B
b�B
bhB
b�B
b�B
b�B
cB
c�B
c�B
d&B
c�B
c�B
dB
d&B
d�B
eB
eB
eFB
ezB
e`B
eFB
e�B
e�B
f2B
f2B
e�B
e�B
fB
f�B
g8B
gB
gB
gB
gB
gRB
gmB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
hXB
hsB
h�B
i�B
j0B
i�B
i�B
jKB
j�B
j�B
kQB
kkB
k�B
k�B
k�B
k�B
lB
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mCB
mCB
mCB
mCB
m�B
n/B
n�B
n�B
o B
oOB
oOB
oOB
oOB
oiB
oOB
o�B
o�B
p�B
p�B
p�B
q'B
qvB
qvB
q�B
rGB
r|B
raB
r|B
r�B
r�B
s3B
s3B
sMB
shB
s�B
s�B
tB
tTB
tTB
t�B
t�B
u�B
u�B
vB
vFB
v+B
v+B
v`B
vzB
v`B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
xB
x�B
x�B
x�B
x�B
y	B
yrB
y�B
yrB
y�B
yrB
yXB
y�B
z�B
z�B
{0B
{JB
{JB
{JB
{JB
{�B
{�B
|PB
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
~BB
~]B
~]B
~BB
~(B
~BB
~(B
~(B
~(B
~(B
~(B
~wB
~�B
.B
.B
~�B
~�B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�4B
��B
��B
�B
�;B
�UB
�oB
�oB
�oB
�UB
��B
�AB
�AB
�uB
��B
��B
��B
��B
�B
��B
�B
�aB
�aB
�G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104843  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172321  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172322  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172322                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022329  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022329  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610121506                      G�O�G�O�G�O�                