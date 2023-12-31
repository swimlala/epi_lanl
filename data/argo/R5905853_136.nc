CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-30T12:50:27Z creation;2022-10-30T12:50:28Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221030125027  20221030130533  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��ҭ�d1   @��(d�@/"��`A��cJ=p��
1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A���A���A�  A�  A�33A�  A���B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bq��Bw��B�  B�  B���B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�ffB�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C233C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @
>@��@��R@��RA\)A?\)A_\)A\)A�z�A�z�A��A��A��GA߮A�z�A��B�
B�
B�
Bp�B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bqp�Bwp�B��B��B��B��B��B�Q�B�Q�B��RB��B��B��B��B��B�Q�B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C�)C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C0]C2(�C3��C5�)C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C`]Cb]Cc�)Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�D���D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��pA��?A���A��3A�˒A��XA��EA���A�̘A��jA��BA�ɺA��UA���A��UA�ZA�EmA�D3A�?HA�EA�`�A�.IA�&�A��A�$A���A� �A��fA�poA֢4A���Aӂ�Aү�A�՛A�x�A�AUA�{A�N�A��.A���A���A��XA���A���A�d&A��;A�{�A�"A��<A�y�A���A���A��A�1A�~�A��A�?�A�}�A��kA���A��DA��&A�jKA��A��A�o5A�xA�I�A��QA���A���A�R�A���A��A�~�A�W�A�o�A��KA���A�S�A�.IA�,qA��jA�ӏA�v+A�A�Y�A|��AzB[Axw2At8�Ao)_AlN�Ai�zAe�HAaE9A]خA\&�AX_pAT��AS�AP �AK\�AH��AF�AE��AC�A>�A<MjA9�A8��A7�"A6��A6��A5��A5hsA4ںA3�A2��A0�A0`�A.qA,�FA+�OA*jA(�A(�A'��A&�zA#��A �ZA��A�AAԕAOvA�IA��Ah�Al�A�Av�A��A�A~�A�-A�A$tA�'A��A��AcAݘA��A�{AQA<�A��A=qA��A �AT�A_A%A�A#:A��A��AS�A@�AzxARTAS�A$A��A�HA\�A�|AC�A��A
]dA	&A�AxlA-�A�hA-�A8�A�bAh�A>BA�QA��Av`A��A ��@�z@���@��*@��j@���@�`�@��@���@��@��@��@�S@�x�@��K@��|@�&@�1�@��@��3@��@�6�@��@���@�|�@��@��K@�A @��@妵@�8@�R@�GE@�+k@��+@��#@��@�H@�H�@�+@�	@ὥ@�_p@�Q�@��"@�z@ߴ�@�J�@޸R@���@ݛ=@��	@�u�@�?�@�x@���@ۛ=@�`B@���@�?�@٧�@�ں@�n�@��@��j@׃{@�x@�a|@�a|@�^5@��@��}@ׯ�@�Mj@֨�@��@Ղ�@�l�@�o@Ӓ:@��)@ѐ�@�<6@�m�@�g�@�j�@�(@ή}@Ξ@Ώ\@΁o@ά�@α�@έ�@Η�@�0U@́�@���@���@̚�@��o@�@ɿH@�X�@���@��@�s�@��E@�M@��P@Ą�@ù�@��@�l"@��@� i@���@��@�iD@�!�@��@���@�� @�$�@�/@���@�z@�=q@��@���@�O@�)_@���@��!@�u@��f@�,�@��@�~(@�,=@�x@��)@��j@�ϫ@�2a@� i@�l"@��@��@���@��@���@�7L@��@�d�@�H@�Ft@��@�Mj@���@���@�`�@�	�@�{J@��@��E@�p;@�:�@���@�/@��@��A@�J�@���@��@���@�i�@�7�@��@��@���@�v`@�^�@�E9@���@���@�q�@�*�@���@���@���@�o @�(�@���@�w�@�K^@��@��m@���@�k�@��@�+�@�9�@���@��*@�c�@�V@��z@��4@��@�x@� �@��a@���@�o @�T�@�?}@���@�W�@�G@���@�N<@�)_@���@��8@��@��/@��@�"h@�  @��
@�rG@�4@�.I@�"�@��@�`�@�!@��r@���@�F�@��K@�W�@�$@���@���@�b�@�7L@�!�@���@���@�n�@�c�@���@�S�@�1�@��@���@���@�Z@�{@��[@�m]@�/�@��@��[@��+@�E�@�/�@��@��m@��N@��^@��@�*0@���@��@�|�@�u@��4@�;d@��@���@�n�@��o@���@�hs@�%@�҉@��j@�z@��@�ݘ@��z@���@�/�@���@��F@�@�@�7@���@���@�e�@���@���@��L@�ff@�5?@��@���@��4@�8�@�q@��@�ȴ@��1@�u�@�1@���@���@�=�@��|@���@��@��q@���@�_p@�33@��@��	@���@�_�@�GE@�$�@���@��@�a@���@�ߤ@���@�{�@�_@�@���@��7@�@O@��@���@�c @�1�@�4@���@��@��@�s@�Vm@��@��P@���@�ں@��u@�2�@��@~��@~a|@~@}�7@}5�@|�)@|�@{�r@{��@{A�@z��@z-@yf�@x��@x��@xU2@x2�@w��@w]�@v�"@v��@vȴ@v�F@v;�@u�.@u�3@u\�@t�I@s�Q@s;d@r��@rW�@r�@q��@q@p��@p �@o��@o'�@n�'@n($@m�#@m��@m8�@l�E@lS�@l-�@lx@k�k@k/�@j��@jGE@i��@i��@i��@i��@i`B@h�e@h?�@g��@g9�@f�'@fE�@e��@eN<@d��@dj@c�g@c�4@c�@b�@bi�@b�@a��@a�=@`�@_�@_��@_/�@^��@^v�@^0U@]��@]x�@]q@\j@[�Q@[��@[�$@[�@Z�m@Y�o@Y�h@Yzx@Ya�@YVm@X�?@X?�@Wݘ@W)_@Vȴ@V�@U��@U<6@T�f@T֡@T�@T�9@T?�@S�&@S��@SMj@S!-@R��@Rȴ@R�F@R}V@R�@Q�@Q�7@Q/@P�P@P��@P�D@PXy@P%�@O�@OZ�@O�@N�b@NJ�@M�Z@M��@M��@Mm]@M0�@L]d@K�@KO@J�@J�}@J�L@J�F@J}V@J5?@I��@Ip�@I0�@H�K@H�D@H �@G�@GZ�@F�@F��@Fv�@F�@Ea�@D��@D�U@Dy>@DA�@C�w@C��@C~�@Cn/@C&@Bߤ@B��@B�+@B@�@B�@A��@A��@A*0@A�@@�@@�@@g8@?��@?S�@>��@>��@>V@=��@=hs@=&�@=q@<�9@<y>@<j@<<�@;�6@;�P@;e�@;@:�<@:��@:)�@9��@9F@8�K@8��@8y>@8V�@8~@7��@7F�@7"�@7�@6�@6!�@5��@5\�@4��@4`�@4%�@3��@3�f@3U�@3�@2҉@2Z�@1�@1�d@1��@1?}@0��@0�Y@0r�@0K^@/�$@/Mj@.�2@.�X@.�h@.l�@.�@-�H@-c�@-�@,�u@,g8@,V�@,:�@,7@+�@+�P@+8@*ں@*z@*V@)�Z@)��@)��@)�M@)T�@(�P@(_@("h@'��@'Y@&�R@&i�@%�@%�M@%a�@%/@$�?@$4n@$G@#��@#��@#� @#��@#J#@"��@"��@"�1@"	@!��@!s�@!L�@!@@ ��@ oi@ �@ 1@�Q@ƨ@�[@�V@qv@�@��@�6@}V@;�@{@�@�d@�C@Vm@/@��@��@w�@�@��@n/@_p@9�@�@�@�s@��@h
@.�@J@�@�@p�@F@=�@-w@�@�@��@e�@U2@Ft@7�@"h@�@��@dZ@F�@1�@(@�s@��@� @p;@@�@.�@&�@�.@�"@}�@hs@T�@Dg@ \@�	@�E@�U@��@��@��@U2@1'@�@��@s@J#@@ȴ@��@v�@@�@��@��@��@o @f�@L�@�@��@�@�e@[�@A�@�W@��@��@y�@P�@�@ i@�"@�"@�@�@��@z@+k@�@�3@��@�@s�@�@��@K^@@�;@��@��@_p@=@,�@o@
�M@
��@
�F@
v�@
@�@
3�@
�@	�@	�T@	�j@	��@	�H@	�=@	f�@	�@�|@��@��@Ĝ@��@�$@�_@|�@y>@tT@_@2�@�&@��@��@~�@b�@E9@9�@$t@(@S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��pA��?A���A��3A�˒A��XA��EA���A�̘A��jA��BA�ɺA��UA���A��UA�ZA�EmA�D3A�?HA�EA�`�A�.IA�&�A��A�$A���A� �A��fA�poA֢4A���Aӂ�Aү�A�՛A�x�A�AUA�{A�N�A��.A���A���A��XA���A���A�d&A��;A�{�A�"A��<A�y�A���A���A��A�1A�~�A��A�?�A�}�A��kA���A��DA��&A�jKA��A��A�o5A�xA�I�A��QA���A���A�R�A���A��A�~�A�W�A�o�A��KA���A�S�A�.IA�,qA��jA�ӏA�v+A�A�Y�A|��AzB[Axw2At8�Ao)_AlN�Ai�zAe�HAaE9A]خA\&�AX_pAT��AS�AP �AK\�AH��AF�AE��AC�A>�A<MjA9�A8��A7�"A6��A6��A5��A5hsA4ںA3�A2��A0�A0`�A.qA,�FA+�OA*jA(�A(�A'��A&�zA#��A �ZA��A�AAԕAOvA�IA��Ah�Al�A�Av�A��A�A~�A�-A�A$tA�'A��A��AcAݘA��A�{AQA<�A��A=qA��A �AT�A_A%A�A#:A��A��AS�A@�AzxARTAS�A$A��A�HA\�A�|AC�A��A
]dA	&A�AxlA-�A�hA-�A8�A�bAh�A>BA�QA��Av`A��A ��@�z@���@��*@��j@���@�`�@��@���@��@��@��@�S@�x�@��K@��|@�&@�1�@��@��3@��@�6�@��@���@�|�@��@��K@�A @��@妵@�8@�R@�GE@�+k@��+@��#@��@�H@�H�@�+@�	@ὥ@�_p@�Q�@��"@�z@ߴ�@�J�@޸R@���@ݛ=@��	@�u�@�?�@�x@���@ۛ=@�`B@���@�?�@٧�@�ں@�n�@��@��j@׃{@�x@�a|@�a|@�^5@��@��}@ׯ�@�Mj@֨�@��@Ղ�@�l�@�o@Ӓ:@��)@ѐ�@�<6@�m�@�g�@�j�@�(@ή}@Ξ@Ώ\@΁o@ά�@α�@έ�@Η�@�0U@́�@���@���@̚�@��o@�@ɿH@�X�@���@��@�s�@��E@�M@��P@Ą�@ù�@��@�l"@��@� i@���@��@�iD@�!�@��@���@�� @�$�@�/@���@�z@�=q@��@���@�O@�)_@���@��!@�u@��f@�,�@��@�~(@�,=@�x@��)@��j@�ϫ@�2a@� i@�l"@��@��@���@��@���@�7L@��@�d�@�H@�Ft@��@�Mj@���@���@�`�@�	�@�{J@��@��E@�p;@�:�@���@�/@��@��A@�J�@���@��@���@�i�@�7�@��@��@���@�v`@�^�@�E9@���@���@�q�@�*�@���@���@���@�o @�(�@���@�w�@�K^@��@��m@���@�k�@��@�+�@�9�@���@��*@�c�@�V@��z@��4@��@�x@� �@��a@���@�o @�T�@�?}@���@�W�@�G@���@�N<@�)_@���@��8@��@��/@��@�"h@�  @��
@�rG@�4@�.I@�"�@��@�`�@�!@��r@���@�F�@��K@�W�@�$@���@���@�b�@�7L@�!�@���@���@�n�@�c�@���@�S�@�1�@��@���@���@�Z@�{@��[@�m]@�/�@��@��[@��+@�E�@�/�@��@��m@��N@��^@��@�*0@���@��@�|�@�u@��4@�;d@��@���@�n�@��o@���@�hs@�%@�҉@��j@�z@��@�ݘ@��z@���@�/�@���@��F@�@�@�7@���@���@�e�@���@���@��L@�ff@�5?@��@���@��4@�8�@�q@��@�ȴ@��1@�u�@�1@���@���@�=�@��|@���@��@��q@���@�_p@�33@��@��	@���@�_�@�GE@�$�@���@��@�a@���@�ߤ@���@�{�@�_@�@���@��7@�@O@��@���@�c @�1�@�4@���@��@��@�s@�Vm@��@��P@���@�ں@��u@�2�@��@~��@~a|@~@}�7@}5�@|�)@|�@{�r@{��@{A�@z��@z-@yf�@x��@x��@xU2@x2�@w��@w]�@v�"@v��@vȴ@v�F@v;�@u�.@u�3@u\�@t�I@s�Q@s;d@r��@rW�@r�@q��@q@p��@p �@o��@o'�@n�'@n($@m�#@m��@m8�@l�E@lS�@l-�@lx@k�k@k/�@j��@jGE@i��@i��@i��@i��@i`B@h�e@h?�@g��@g9�@f�'@fE�@e��@eN<@d��@dj@c�g@c�4@c�@b�@bi�@b�@a��@a�=@`�@_�@_��@_/�@^��@^v�@^0U@]��@]x�@]q@\j@[�Q@[��@[�$@[�@Z�m@Y�o@Y�h@Yzx@Ya�@YVm@X�?@X?�@Wݘ@W)_@Vȴ@V�@U��@U<6@T�f@T֡@T�@T�9@T?�@S�&@S��@SMj@S!-@R��@Rȴ@R�F@R}V@R�@Q�@Q�7@Q/@P�P@P��@P�D@PXy@P%�@O�@OZ�@O�@N�b@NJ�@M�Z@M��@M��@Mm]@M0�@L]d@K�@KO@J�@J�}@J�L@J�F@J}V@J5?@I��@Ip�@I0�@H�K@H�D@H �@G�@GZ�@F�@F��@Fv�@F�@Ea�@D��@D�U@Dy>@DA�@C�w@C��@C~�@Cn/@C&@Bߤ@B��@B�+@B@�@B�@A��@A��@A*0@A�@@�@@�@@g8@?��@?S�@>��@>��@>V@=��@=hs@=&�@=q@<�9@<y>@<j@<<�@;�6@;�P@;e�@;@:�<@:��@:)�@9��@9F@8�K@8��@8y>@8V�@8~@7��@7F�@7"�@7�@6�@6!�@5��@5\�@4��@4`�@4%�@3��@3�f@3U�@3�@2҉@2Z�@1�@1�d@1��@1?}@0��@0�Y@0r�@0K^@/�$@/Mj@.�2@.�X@.�h@.l�@.�@-�H@-c�@-�@,�u@,g8@,V�@,:�@,7@+�@+�P@+8@*ں@*z@*V@)�Z@)��@)��@)�M@)T�@(�P@(_@("h@'��@'Y@&�R@&i�@%�@%�M@%a�@%/@$�?@$4n@$G@#��@#��@#� @#��@#J#@"��@"��@"�1@"	@!��@!s�@!L�@!@@ ��@ oi@ �@ 1@�Q@ƨ@�[@�V@qv@�@��@�6@}V@;�@{@�@�d@�C@Vm@/@��@��@w�@�@��@n/@_p@9�@�@�@�s@��@h
@.�@J@�@�@p�@F@=�@-w@�@�@��@e�@U2@Ft@7�@"h@�@��@dZ@F�@1�@(@�s@��@� @p;@@�@.�@&�@�.@�"@}�@hs@T�@Dg@ \@�	@�E@�U@��@��@��@U2@1'@�@��@s@J#@@ȴ@��@v�@@�@��@��@��@o @f�@L�@�@��@�@�e@[�@A�@�W@��@��@y�@P�@�@ i@�"@�"@�@�@��@z@+k@�@�3@��@�@s�@�@��@K^@@�;@��@��@_p@=@,�@o@
�M@
��@
�F@
v�@
@�@
3�@
�@	�@	�T@	�j@	��@	�H@	�=@	f�@	�@�|@��@��@Ĝ@��@�$@�_@|�@y>@tT@_@2�@�&@��@��@~�@b�@E9@9�@$t@(@S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
0!B
0�B
0�B
0�B
0UB
0�B
/�B
0�B
4B
4B
4B
4�B
5�B
5tB
5�B
<�B
?cB
A;B
BAB
R�B
�B
�nB
��B{BDB�B
��B
��B3B B
�$B
�B
�;B
�B
�3BmBB�B-�B�GB�eB$B1B	�BMB��B��B�B��B��B��B�B�jB�9B�B�)B�yB��B�TBٚB�KBѷB��BΊBƎB��B�@B�-B��B�9B��B]�B>�B)_BB
��B
�sB
�NB
�jB
� B
�FB
��B
��B
�6B
s�B
RB
 �B	��B	�B	�!B	�1B	��B	��B	�B	yrB	_VB	O(B	B�B	,�B	B	�B�}B�	B�	B�FB�B�B�B�XB��B�YB�@B�dB��B�QB��B�;B��B��B��B��B��B	�B	EB	#B	$&B	)�B	6B	9rB	,B	�B	#B	�B	(�B	,�B	33B	4�B	33B	2-B	-�B	-wB	/B	2�B	-�B	B	�B	B	�B	/OB	5�B	I�B	gB	c�B	k�B	��B	�_B	�#B	�&B	�	B	�GB	��B	��B	��B	��B	�LB	�4B	�&B	רB	׍B	ӏB	��B	�B	�6B	��B	�{B	�9B	�MB	��B	�B	��B	�DB	��B	�eB	�B	��B	�`B	��B	�B	��B	�B	�!B	�KB	�|B	�FB	�B	� B	�8B	�B	�yB	��B	�>B	��B	��B	��B	��B	�>B	��B	��B	�B	�LB	�dB	��B	B	��B	�B	�XB	��B	��B	�>B	�*B	��B	��B	�xB	��B	��B	�B	�VB	��B	��B	��B	��B	�B	��B	��B	��B	ŢB	��B	��B	�1B	ɺB	�XB	�DB	�^B	�JB	�B	̳B	͹B	�"B	�<B	�VB	�VB	ΊB	ΊB	ΥB	οB	��B	�B	�B	ΥB	�vB	�oB	�#B	��B	�nB	�nB	�B	�zB	�mB	�B	�B	�B	�$B	�B	�>B	��B	�hB	��B	�pB	�B	ޞB	��B	�B	�8B	��B	�
B	�B	��B	��B	��B	�cB	�OB	�B	� B	�'B	�AB	�;B	�B	�CB	��B	��B	�B	�;B	�iB	�cB	�/B	�/B	�/B	��B	��B	�B	��B	�CB	�wB	�wB	��B	��B	��B	�B	��B	�!B	��B	��B	�AB	�vB	��B	��B	�-B	�GB	�aB	�B	�nB	�%B	�tB	�FB	�B	�2B	��B	�B	��B	��B	��B	��B	�PB	��B	�B	��B	�B	��B	�(B	��B	��B	�]B	�]B	�wB	��B	��B	��B	��B	��B	��B	��B	�]B	�(B	�wB	��B	��B	��B	�(B	�B	�wB	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�B	�B	�B	��B	��B	�]B	�BB	��B
 iB
 �B
 iB
;B
�B
[B
'B
oB
B
�B
�B
�B
�B

	B
�B
bB
�B
�B
�B
�B
 B
�B
TB
TB
�B
�B
FB
�B
gB
�B
�B
MB
�B
MB
2B
�B
�B
�B
�B
�B
MB
B
9B
B
9B
�B
�B

B
mB
�B
�B
�B
�B
�B
�B
�B
�B
eB
eB
�B
�B
�B
�B
�B
7B
kB
�B
	B
qB
�B
�B
)B
B
�B
�B
/B
B
IB
~B
/B
�B
�B
5B
�B
�B
�B
VB
�B
 B
 'B
 �B
 �B
 �B
!B
!bB
!�B
!�B
!�B
"�B
"�B
# B
#:B
#:B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%`B
%�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
(>B
(�B
(�B
)�B
*B
+B
+B
+B
*�B
*�B
+B
+�B
+�B
,=B
,WB
,�B
-)B
-�B
-�B
.B
-�B
-�B
.}B
.�B
/ B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
2aB
2|B
2aB
2aB
2�B
33B
4B
4�B
4�B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
6B
6`B
6zB
6zB
6zB
6�B
7LB
8B
7�B
7�B
7�B
7�B
8B
8�B
9	B
9XB
9XB
9�B
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;B
;0B
;JB
;�B
<B
<jB
<�B
="B
=qB
=�B
>(B
>B
>]B
>�B
>�B
?HB
?cB
?cB
?�B
@B
@�B
@�B
AoB
A�B
BB
BB
BB
B�B
CB
CB
CaB
C�B
DB
DB
C�B
C�B
C�B
DMB
DgB
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
GEB
HB
H1B
H�B
H�B
H�B
I7B
J	B
J�B
KxB
K�B
KDB
LB
L0B
K�B
K�B
K�B
K�B
LJB
L~B
L~B
MB
MB
L�B
M�B
NB
NVB
N�B
N�B
N�B
OB
O\B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
PbB
P�B
P�B
Q B
Q B
QhB
QNB
QNB
QhB
Q�B
Q�B
Q�B
R:B
RTB
R�B
R�B
RoB
RoB
RoB
S@B
S�B
T,B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
VB
VB
VSB
V�B
V�B
V�B
W$B
WYB
W?B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X_B
X�B
YB
YKB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\CB
\xB
]B
\�B
]B
]/B
^OB
^jB
^jB
^�B
^�B
_!B
_pB
_�B
_pB
_�B
_�B
`�B
`�B
aHB
a|B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
c B
cnB
cnB
c�B
d@B
dZB
d�B
ezB
e`B
e`B
e`B
e�B
f2B
fB
fB
f�B
gB
gRB
gRB
gRB
g�B
h$B
hXB
hsB
hXB
h�B
h�B
iB
i_B
i�B
j0B
jKB
jKB
jB
jB
j�B
j�B
kB
k�B
k�B
k�B
l"B
l=B
l=B
lWB
l=B
l�B
l�B
mB
mwB
m�B
nB
n/B
n}B
n�B
n�B
n�B
oOB
o�B
o�B
pB
pB
pB
p;B
pUB
p�B
p�B
p�B
qvB
q�B
q�B
rB
r-B
r|B
r�B
r�B
r�B
s3B
sMB
sMB
shB
s�B
s�B
s�B
tB
tB
tnB
tnB
t�B
t�B
t�B
uZB
u?B
utB
u�B
u�B
v+B
v�B
wB
wB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
y>B
yXB
yXB
yrB
yrB
yrB
y�B
zB
z*B
z*B
zDB
zxB
z�B
z�B
z�B
{B
{JB
{B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}qB
}�B
}�B
}�B
~BB
~]B
~wB
~�B
~�B
HB
cB
�B
�B
�B
� B
�OB
�4B
�B
��B
��B
�;B
�oB
��B
��B
��B
�AB
�AB
�AB
�AB
�AB
�'B
�uB
��B
��B
�-B
�{B
�{B
�aB
��B
�gB
��B
��B
�B
�SB
�SB
��B
��B
��B
��B
��B
�B
�?B
�tB
��B
��B
��B
��B
�B
��B
��B
��B
�+B
�+B
�zB
��B
��B
�1B
�B
�B
�1B
�B
�KB
�KB
�1B
�1B
�KB
��B
��B
��B
��B
�B
�7B
�RB
�lB
��B
�	B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
0!B
0�B
0�B
0�B
0UB
0�B
/�B
0�B
4B
4B
4B
4�B
5�B
5tB
5�B
<�B
?cB
A;B
BAB
R�B
�B
�nB
��B{BDB�B
��B
��B3B B
�$B
�B
�;B
�B
�3BmBB�B-�B�GB�eB$B1B	�BMB��B��B�B��B��B��B�B�jB�9B�B�)B�yB��B�TBٚB�KBѷB��BΊBƎB��B�@B�-B��B�9B��B]�B>�B)_BB
��B
�sB
�NB
�jB
� B
�FB
��B
��B
�6B
s�B
RB
 �B	��B	�B	�!B	�1B	��B	��B	�B	yrB	_VB	O(B	B�B	,�B	B	�B�}B�	B�	B�FB�B�B�B�XB��B�YB�@B�dB��B�QB��B�;B��B��B��B��B��B	�B	EB	#B	$&B	)�B	6B	9rB	,B	�B	#B	�B	(�B	,�B	33B	4�B	33B	2-B	-�B	-wB	/B	2�B	-�B	B	�B	B	�B	/OB	5�B	I�B	gB	c�B	k�B	��B	�_B	�#B	�&B	�	B	�GB	��B	��B	��B	��B	�LB	�4B	�&B	רB	׍B	ӏB	��B	�B	�6B	��B	�{B	�9B	�MB	��B	�B	��B	�DB	��B	�eB	�B	��B	�`B	��B	�B	��B	�B	�!B	�KB	�|B	�FB	�B	� B	�8B	�B	�yB	��B	�>B	��B	��B	��B	��B	�>B	��B	��B	�B	�LB	�dB	��B	B	��B	�B	�XB	��B	��B	�>B	�*B	��B	��B	�xB	��B	��B	�B	�VB	��B	��B	��B	��B	�B	��B	��B	��B	ŢB	��B	��B	�1B	ɺB	�XB	�DB	�^B	�JB	�B	̳B	͹B	�"B	�<B	�VB	�VB	ΊB	ΊB	ΥB	οB	��B	�B	�B	ΥB	�vB	�oB	�#B	��B	�nB	�nB	�B	�zB	�mB	�B	�B	�B	�$B	�B	�>B	��B	�hB	��B	�pB	�B	ޞB	��B	�B	�8B	��B	�
B	�B	��B	��B	��B	�cB	�OB	�B	� B	�'B	�AB	�;B	�B	�CB	��B	��B	�B	�;B	�iB	�cB	�/B	�/B	�/B	��B	��B	�B	��B	�CB	�wB	�wB	��B	��B	��B	�B	��B	�!B	��B	��B	�AB	�vB	��B	��B	�-B	�GB	�aB	�B	�nB	�%B	�tB	�FB	�B	�2B	��B	�B	��B	��B	��B	��B	�PB	��B	�B	��B	�B	��B	�(B	��B	��B	�]B	�]B	�wB	��B	��B	��B	��B	��B	��B	��B	�]B	�(B	�wB	��B	��B	��B	�(B	�B	�wB	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�B	�B	�B	��B	��B	�]B	�BB	��B
 iB
 �B
 iB
;B
�B
[B
'B
oB
B
�B
�B
�B
�B

	B
�B
bB
�B
�B
�B
�B
 B
�B
TB
TB
�B
�B
FB
�B
gB
�B
�B
MB
�B
MB
2B
�B
�B
�B
�B
�B
MB
B
9B
B
9B
�B
�B

B
mB
�B
�B
�B
�B
�B
�B
�B
�B
eB
eB
�B
�B
�B
�B
�B
7B
kB
�B
	B
qB
�B
�B
)B
B
�B
�B
/B
B
IB
~B
/B
�B
�B
5B
�B
�B
�B
VB
�B
 B
 'B
 �B
 �B
 �B
!B
!bB
!�B
!�B
!�B
"�B
"�B
# B
#:B
#:B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%`B
%�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
(>B
(�B
(�B
)�B
*B
+B
+B
+B
*�B
*�B
+B
+�B
+�B
,=B
,WB
,�B
-)B
-�B
-�B
.B
-�B
-�B
.}B
.�B
/ B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
2aB
2|B
2aB
2aB
2�B
33B
4B
4�B
4�B
4�B
4�B
5ZB
5�B
5�B
5�B
5�B
6B
6`B
6zB
6zB
6zB
6�B
7LB
8B
7�B
7�B
7�B
7�B
8B
8�B
9	B
9XB
9XB
9�B
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;B
;0B
;JB
;�B
<B
<jB
<�B
="B
=qB
=�B
>(B
>B
>]B
>�B
>�B
?HB
?cB
?cB
?�B
@B
@�B
@�B
AoB
A�B
BB
BB
BB
B�B
CB
CB
CaB
C�B
DB
DB
C�B
C�B
C�B
DMB
DgB
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
GEB
HB
H1B
H�B
H�B
H�B
I7B
J	B
J�B
KxB
K�B
KDB
LB
L0B
K�B
K�B
K�B
K�B
LJB
L~B
L~B
MB
MB
L�B
M�B
NB
NVB
N�B
N�B
N�B
OB
O\B
O�B
O�B
O�B
O�B
O�B
P.B
P.B
PbB
P�B
P�B
Q B
Q B
QhB
QNB
QNB
QhB
Q�B
Q�B
Q�B
R:B
RTB
R�B
R�B
RoB
RoB
RoB
S@B
S�B
T,B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
VB
VB
VSB
V�B
V�B
V�B
W$B
WYB
W?B
W?B
WsB
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X_B
X�B
YB
YKB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\CB
\xB
]B
\�B
]B
]/B
^OB
^jB
^jB
^�B
^�B
_!B
_pB
_�B
_pB
_�B
_�B
`�B
`�B
aHB
a|B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
c B
cnB
cnB
c�B
d@B
dZB
d�B
ezB
e`B
e`B
e`B
e�B
f2B
fB
fB
f�B
gB
gRB
gRB
gRB
g�B
h$B
hXB
hsB
hXB
h�B
h�B
iB
i_B
i�B
j0B
jKB
jKB
jB
jB
j�B
j�B
kB
k�B
k�B
k�B
l"B
l=B
l=B
lWB
l=B
l�B
l�B
mB
mwB
m�B
nB
n/B
n}B
n�B
n�B
n�B
oOB
o�B
o�B
pB
pB
pB
p;B
pUB
p�B
p�B
p�B
qvB
q�B
q�B
rB
r-B
r|B
r�B
r�B
r�B
s3B
sMB
sMB
shB
s�B
s�B
s�B
tB
tB
tnB
tnB
t�B
t�B
t�B
uZB
u?B
utB
u�B
u�B
v+B
v�B
wB
wB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
y>B
yXB
yXB
yrB
yrB
yrB
y�B
zB
z*B
z*B
zDB
zxB
z�B
z�B
z�B
{B
{JB
{B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}qB
}�B
}�B
}�B
~BB
~]B
~wB
~�B
~�B
HB
cB
�B
�B
�B
� B
�OB
�4B
�B
��B
��B
�;B
�oB
��B
��B
��B
�AB
�AB
�AB
�AB
�AB
�'B
�uB
��B
��B
�-B
�{B
�{B
�aB
��B
�gB
��B
��B
�B
�SB
�SB
��B
��B
��B
��B
��B
�B
�?B
�tB
��B
��B
��B
��B
�B
��B
��B
��B
�+B
�+B
�zB
��B
��B
�1B
�B
�B
�1B
�B
�KB
�KB
�1B
�1B
�KB
��B
��B
��B
��B
�B
�7B
�RB
�lB
��B
�	B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221030125026  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221030125027  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221030125028  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221030125028                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221030215032  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221030215032  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221030130533                      G�O�G�O�G�O�                