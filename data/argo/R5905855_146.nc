CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-12T18:43:58Z creation;2023-02-12T18:44:00Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230212184358  20230212185918  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�f�\�1   @�f�w`@/�n��O��d*ȴ9X1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw33B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B���B�  B�  B�  B�  B�33B���B���C   C  C  C  C  C
  C�C33C� C�fC�fC  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @
>@}p�@��R@��RAA=A_\)A\)A��A��A��A��GAϮA߮A�A��B�
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
Bp=pBw
=Bp�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��RB��C��C��C��C��C	��C]C(�Cu�C�)C�)C��C��C��C��C��C��C!��C$]C%��C'��C)��C+��C-��C/��C1��C3��C5�)C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CX]CZ]C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDEwDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�D���D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�;�D�q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�A��A��A��A��A��A��A��A�A��A��A�VA�!A��A��A��A� 'A�!-A�!�A�"�A�$@A�%A�&LA�'A�'�A�'RA�&�A�&�A�&�A��]A˅�A�PHA�B�A�9�A�-�A�%A��A��A��A��A�A��A�  A���A��5A��?A�-A��sA�:^A�$@A���A��1A�̘A��zA���A���A�@�A��1A�v�A��vA���A���A�`vA���A��A���A�w2A�m)A�f2A�%�A���A���A�Z�A��A��4A��CA���A~9XAw�kAt�9Ar�[Aq2aAp;dAh˒Ad�A]��A[�AZ��AZ�AX�aAT��AS��AQ��AP�AN�0AN�AM��AK�vAH֡AFJAB��AA�A>|�A=J�A<b�A;�eA9�DA5E9A3ںA2�A2�uA1�A1GA0�A-6zA*�A)g�A(~�A'jA&_A%�9A$�A#�A#RTA"��A"��A!��A S&A!�A'8�A(a|A'�7A$�A$>BA$0�A$��A%^5A%o A%�A$�0A$5�A#s�A"_A!�A�A5?A#�Ae,A��Aw2A��ARTA��A��ASA�mA0�A��A�A1�Av`A�AqvA6A�1AH�A�AXA�3A�CA��AsAg8A4A4AϫACA�A�mA1�A	��A0�A��Ab�AK^A�A��A֡A��A�.Av�AL0AJA�AkQA�oA^5A��A��A�uAe�A�A�A'RA�A��A  A dZ@�rG@��~@��@�	l@�l�@���@�'@�Ɇ@��D@��@�}�@��@�Ov@��y@���@�($@��@땁@���@�\�@��@�F�@���@�	@��m@�@�z@��r@���@�o@�)�@ᴢ@�O�@���@���@�_�@�
�@���@ߝ�@�Vm@�	l@�u%@��d@�8@ܶ�@�\�@���@�o @���@�H@���@�Vm@�͟@�{@��@�|�@�m]@�&�@��@��v@ֿ�@֕�@��@Ձ�@�o@Դ9@��@�t�@�#�@��]@ғu@�w�@�M�@Ѯ@���@�@��]@�xl@��6@�IR@�@��'@Α�@��@�Vm@�Ft@�%�@�IR@�v�@��;@̊r@��+@��#@ˏ�@ʢ4@��@�b�@�>�@��y@ȃ�@�3�@���@�ԕ@���@���@�tT@� \@Į}@�q�@�/�@��W@��3@ð�@Ü�@�e,@�,�@��@�_@��@���@��*@�(�@�+@�V@�@�j@���@�w2@�f�@�33@�/@�&@�;@��@�{J@��@�ݘ@���@��T@�g8@���@��f@�u�@�a@���@�2�@�	@���@��@��@�:*@���@��Q@��@��@���@�4n@��n@�-w@��@�a@��@��@�W?@���@���@�"�@��@���@��h@��@��H@���@�ϫ@�K�@��@��,@���@��_@�+k@��@��	@��$@�*�@�_@�ϫ@��"@�T�@�7L@�C@���@��6@��@��@�<�@��t@��)@�S�@��@���@�k�@�!�@���@��M@���@�M�@���@���@�&@���@�3�@�@���@��q@�\)@�A @�1�@�W?@�\)@��@���@��@�6�@�6@�~@��@��g@���@��M@��M@�a@�6z@�V@���@���@�YK@�M@�Z@�-@���@��X@� �@��"@��@�h�@�<�@��g@�hs@��M@��\@�E�@���@��@�9�@��8@��}@�E�@�ԕ@��@�W?@���@���@���@���@���@�?�@��K@�4�@���@�z�@�YK@�7@�ݘ@���@���@�f�@���@�ѷ@���@��+@�)�@���@���@�o�@� \@���@��@�Q�@�*�@��@��@���@��k@�x�@�Vm@��@��	@���@��$@�Q@��@�qv@�Vm@��@��9@���@�v�@��W@��@�f�@�^�@�=@�)_@��@�@��	@�i�@�@��@���@�j�@�4@��4@��;@�y�@��@��O@���@��@�|�@�ff@��@�4@�	l@��@��E@���@�1�@��>@��[@��V@�`B@��@���@��9@�z�@ݘ@33@~�m@~�@}�~@}�@|�@|Q�@{j�@{�@z�c@z�\@ze@yx�@y+@x�@xD�@w�]@wخ@w�$@v�"@vM�@u��@u:�@u%F@t�Y@s�A@s�}@sqv@r��@q�)@q�@p�.@p/�@o�K@o�@oO@n��@na|@m��@m+�@l�|@l��@lV�@l'R@k�]@k�}@k��@kW?@j��@j)�@i��@i#�@hU2@g��@g� @g��@gZ�@gS@f�!@fa|@e��@e+�@e�@dI�@d�@c�f@b�@b�+@a��@`�[@`6@_��@_S�@^�R@^xl@]�@]��@]*0@\��@\bN@\	�@[��@[a@Z��@Z��@Z��@Z�@Y��@Y%@X�j@W��@W��@W_p@WK�@WK�@W33@W33@W&@W�@V��@V@U�@U��@U��@U��@UrG@T��@T�I@S�0@Sg�@S)_@SMj@S@O@S@O@R��@R��@RGE@R!�@Q��@Q@Q@P�|@P��@P'R@O�@O�*@Oa@O�@N�1@Nn�@N�@M��@L�U@L�.@L[�@K�P@J�B@J�6@J�\@I�)@I��@I\�@I@HɆ@H�@Gy�@GF�@G�@Fz@F($@E�#@E��@D�@D"h@Cݘ@C�@C��@CU�@B�m@B-@Aa�@@ѷ@@:�@?�@?H�@?Y@>�c@>��@>}V@>GE@>0U@>O@>@>�@=��@=�#@=�@=o @<�|@<l"@;�@;�g@;o�@;�@:��@:��@:�s@:�<@:�h@:��@:�@:��@:R�@:J@9��@9�@8q@7�;@7�@7�	@7�{@7+@6-@5�@5a�@5@4�@4֡@4V�@4~@3�W@3��@3v`@39�@3�@3�@3�@3�@3�@2�M@2�B@1��@1�^@1��@1�'@1�"@1`B@1�@0��@0�O@0�@0y>@0�@/�Q@/��@/��@/��@/�@@/�@/��@/��@/qv@/O@.�c@.��@.��@.c @.C�@-�j@-�"@-}�@-/@,�@,�E@,�@,j@,�@+�w@+S�@+@*ȴ@*�x@*��@*�A@*s�@*5?@)��@)�@(��@(K^@(�@'ݘ@'g�@&��@&��@&�L@&��@&s�@&R�@&;�@%��@%�S@%j@%Vm@%�@$��@$u�@$%�@$1@#�6@#j�@"�H@"n�@"Ov@"J@!��@!��@!B�@ ��@ ��@ �o@ c�@ Xy@ :�@��@�@@n/@�@�}@��@�6@�b@$�@�=@w2@f�@[W@O�@Dg@0�@��@�E@��@�9@��@4n@o@_�@�o@��@s�@N<@8�@*0@�	@�@��@�@�@�@�E@�U@�D@PH@��@K�@��@��@�H@ں@҉@��@�1@l�@�@��@o @a�@X@L�@q@/�@�@�@�@1@�+@�W@�w@v`@)_@�c@��@��@@�@	@�D@��@��@A @�@�o@e�@`�@]d@U2@Ft@:�@4n@,=@'R@7@@1@��@��@��@>�@{�@Z�@0U@�@ �@�d@�t@�M@J�@q@�@�/@�o@@��@��@��@�*@��@e�@@O@@
�@
�@
�y@
��@
?@
�@
�@	�D@	��@	�Z@	��@	ԕ@	�t@	Q�@	*0@	�@��@�@q@�r@�@P�@&@�@ߤ@��@��@��@�r1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��A�A��A��A��A��A��A��A��A�A��A��A�VA�!A��A��A��A� 'A�!-A�!�A�"�A�$@A�%A�&LA�'A�'�A�'RA�&�A�&�A�&�A��]A˅�A�PHA�B�A�9�A�-�A�%A��A��A��A��A�A��A�  A���A��5A��?A�-A��sA�:^A�$@A���A��1A�̘A��zA���A���A�@�A��1A�v�A��vA���A���A�`vA���A��A���A�w2A�m)A�f2A�%�A���A���A�Z�A��A��4A��CA���A~9XAw�kAt�9Ar�[Aq2aAp;dAh˒Ad�A]��A[�AZ��AZ�AX�aAT��AS��AQ��AP�AN�0AN�AM��AK�vAH֡AFJAB��AA�A>|�A=J�A<b�A;�eA9�DA5E9A3ںA2�A2�uA1�A1GA0�A-6zA*�A)g�A(~�A'jA&_A%�9A$�A#�A#RTA"��A"��A!��A S&A!�A'8�A(a|A'�7A$�A$>BA$0�A$��A%^5A%o A%�A$�0A$5�A#s�A"_A!�A�A5?A#�Ae,A��Aw2A��ARTA��A��ASA�mA0�A��A�A1�Av`A�AqvA6A�1AH�A�AXA�3A�CA��AsAg8A4A4AϫACA�A�mA1�A	��A0�A��Ab�AK^A�A��A֡A��A�.Av�AL0AJA�AkQA�oA^5A��A��A�uAe�A�A�A'RA�A��A  A dZ@�rG@��~@��@�	l@�l�@���@�'@�Ɇ@��D@��@�}�@��@�Ov@��y@���@�($@��@땁@���@�\�@��@�F�@���@�	@��m@�@�z@��r@���@�o@�)�@ᴢ@�O�@���@���@�_�@�
�@���@ߝ�@�Vm@�	l@�u%@��d@�8@ܶ�@�\�@���@�o @���@�H@���@�Vm@�͟@�{@��@�|�@�m]@�&�@��@��v@ֿ�@֕�@��@Ձ�@�o@Դ9@��@�t�@�#�@��]@ғu@�w�@�M�@Ѯ@���@�@��]@�xl@��6@�IR@�@��'@Α�@��@�Vm@�Ft@�%�@�IR@�v�@��;@̊r@��+@��#@ˏ�@ʢ4@��@�b�@�>�@��y@ȃ�@�3�@���@�ԕ@���@���@�tT@� \@Į}@�q�@�/�@��W@��3@ð�@Ü�@�e,@�,�@��@�_@��@���@��*@�(�@�+@�V@�@�j@���@�w2@�f�@�33@�/@�&@�;@��@�{J@��@�ݘ@���@��T@�g8@���@��f@�u�@�a@���@�2�@�	@���@��@��@�:*@���@��Q@��@��@���@�4n@��n@�-w@��@�a@��@��@�W?@���@���@�"�@��@���@��h@��@��H@���@�ϫ@�K�@��@��,@���@��_@�+k@��@��	@��$@�*�@�_@�ϫ@��"@�T�@�7L@�C@���@��6@��@��@�<�@��t@��)@�S�@��@���@�k�@�!�@���@��M@���@�M�@���@���@�&@���@�3�@�@���@��q@�\)@�A @�1�@�W?@�\)@��@���@��@�6�@�6@�~@��@��g@���@��M@��M@�a@�6z@�V@���@���@�YK@�M@�Z@�-@���@��X@� �@��"@��@�h�@�<�@��g@�hs@��M@��\@�E�@���@��@�9�@��8@��}@�E�@�ԕ@��@�W?@���@���@���@���@���@�?�@��K@�4�@���@�z�@�YK@�7@�ݘ@���@���@�f�@���@�ѷ@���@��+@�)�@���@���@�o�@� \@���@��@�Q�@�*�@��@��@���@��k@�x�@�Vm@��@��	@���@��$@�Q@��@�qv@�Vm@��@��9@���@�v�@��W@��@�f�@�^�@�=@�)_@��@�@��	@�i�@�@��@���@�j�@�4@��4@��;@�y�@��@��O@���@��@�|�@�ff@��@�4@�	l@��@��E@���@�1�@��>@��[@��V@�`B@��@���@��9@�z�@ݘ@33@~�m@~�@}�~@}�@|�@|Q�@{j�@{�@z�c@z�\@ze@yx�@y+@x�@xD�@w�]@wخ@w�$@v�"@vM�@u��@u:�@u%F@t�Y@s�A@s�}@sqv@r��@q�)@q�@p�.@p/�@o�K@o�@oO@n��@na|@m��@m+�@l�|@l��@lV�@l'R@k�]@k�}@k��@kW?@j��@j)�@i��@i#�@hU2@g��@g� @g��@gZ�@gS@f�!@fa|@e��@e+�@e�@dI�@d�@c�f@b�@b�+@a��@`�[@`6@_��@_S�@^�R@^xl@]�@]��@]*0@\��@\bN@\	�@[��@[a@Z��@Z��@Z��@Z�@Y��@Y%@X�j@W��@W��@W_p@WK�@WK�@W33@W33@W&@W�@V��@V@U�@U��@U��@U��@UrG@T��@T�I@S�0@Sg�@S)_@SMj@S@O@S@O@R��@R��@RGE@R!�@Q��@Q@Q@P�|@P��@P'R@O�@O�*@Oa@O�@N�1@Nn�@N�@M��@L�U@L�.@L[�@K�P@J�B@J�6@J�\@I�)@I��@I\�@I@HɆ@H�@Gy�@GF�@G�@Fz@F($@E�#@E��@D�@D"h@Cݘ@C�@C��@CU�@B�m@B-@Aa�@@ѷ@@:�@?�@?H�@?Y@>�c@>��@>}V@>GE@>0U@>O@>@>�@=��@=�#@=�@=o @<�|@<l"@;�@;�g@;o�@;�@:��@:��@:�s@:�<@:�h@:��@:�@:��@:R�@:J@9��@9�@8q@7�;@7�@7�	@7�{@7+@6-@5�@5a�@5@4�@4֡@4V�@4~@3�W@3��@3v`@39�@3�@3�@3�@3�@3�@2�M@2�B@1��@1�^@1��@1�'@1�"@1`B@1�@0��@0�O@0�@0y>@0�@/�Q@/��@/��@/��@/�@@/�@/��@/��@/qv@/O@.�c@.��@.��@.c @.C�@-�j@-�"@-}�@-/@,�@,�E@,�@,j@,�@+�w@+S�@+@*ȴ@*�x@*��@*�A@*s�@*5?@)��@)�@(��@(K^@(�@'ݘ@'g�@&��@&��@&�L@&��@&s�@&R�@&;�@%��@%�S@%j@%Vm@%�@$��@$u�@$%�@$1@#�6@#j�@"�H@"n�@"Ov@"J@!��@!��@!B�@ ��@ ��@ �o@ c�@ Xy@ :�@��@�@@n/@�@�}@��@�6@�b@$�@�=@w2@f�@[W@O�@Dg@0�@��@�E@��@�9@��@4n@o@_�@�o@��@s�@N<@8�@*0@�	@�@��@�@�@�@�E@�U@�D@PH@��@K�@��@��@�H@ں@҉@��@�1@l�@�@��@o @a�@X@L�@q@/�@�@�@�@1@�+@�W@�w@v`@)_@�c@��@��@@�@	@�D@��@��@A @�@�o@e�@`�@]d@U2@Ft@:�@4n@,=@'R@7@@1@��@��@��@>�@{�@Z�@0U@�@ �@�d@�t@�M@J�@q@�@�/@�o@@��@��@��@�*@��@e�@@O@@
�@
�@
�y@
��@
?@
�@
�@	�D@	��@	�Z@	��@	ԕ@	�t@	Q�@	*0@	�@��@�@q@�r@�@P�@&@�@ߤ@��@��@��@�r1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	abB	abB	a-B	abB	abB	abB	aB	a-B	aB	aB	aB	`�B	a-B	aB	a-B	aB	aB	aHB	abB	a-B	aB	aB	`�B	aB	`�B	`�B	aB	a�B	cnB	b�B	a�B	��B
B
-CB
5?B
6`B
6�B
7B
7�B
8RB
9>B
:*B
:�B
;JB
<B
=qB
@ B
C�B
AB
O�B
��B
��B
�WB
�&B
��B�B8�BY�B`�Bk�BaHBT{B*�B#B%FB �B�B
�cB
��B
��B
G�B
1�B
1�B
QB	��B	�B	��B	�JB	�B	�B	�kB	�~B	��B	�:B	�6B	y�B	h�B	Y1B	MPB	LJB	KB	I�B	G�B	G�B	J�B	N�B	MPB	J�B	KxB	P�B	K�B	IlB	@�B	K)B	N"B	E�B	>BB	3B	"�B	B	%`B	/B	6�B	<B	=�B	?B	3B	0�B	(sB	(�B	*�B	(�B	&B	"4B	�B	"�B	$tB	6FB	M�B	Y1B	j�B	�4B	�B	�B	�=B	�)B	��B	�RB
�B
B
B
�B

�B
9B
 B	��B	�B	ªB	��B	��B	�"B	��B	�]B	��B	��B	��B	�{B	�AB	�B	~�B	��B	�B	��B	��B	��B	�~B	��B	��B	��B	бB	��B	��B	�B	��B	�2B	�B	�,B	�,B	�bB	��B	�]B	�|B	�bB	�OB	��B	�$B	��B	��B	ɺB	��B	�:B	ѝB	�4B	�.B	οB	�6B	�B	��B	�B	רB	��B	ԕB	ϑB	��B	�B	��B	�jB	��B	��B	ƨB	��B	�iB	��B	�$B	�rB	��B	�FB	�zB	�B	��B	�2B	�8B	��B	�dB	�OB	�B	��B	ÖB	�B	�+B	�EB	��B	�B	ɆB	��B	��B	�#B	�XB	��B	��B	��B	ЗB	��B	бB	��B	�hB	��B	� B	�oB	�B	��B	�B	ӏB	ԯB	�gB	�B	�
B	�sB	�B	��B	�B	�B	�#B	�B	��B	�)B	ܒB	ݘB	��B	ݲB	��B	��B	�jB	��B	��B	��B	��B	��B	�B	�&B	��B	��B	��B	�>B	��B	��B	�)B	�qB	�B	�B	�sB	��B	�B	�CB	�!B	�B	�lB	�B	�nB	��B	��B	�UB	�B	�B	��B	�-B	�B	�	B	�XB	�	B	��B	��B	��B	�JB	�B	�^B	�8B	��B	�fB	�lB	�>B	�rB	��B	��B	�^B	��B	�^B	��B	��B	�	B	��B	��B	��B	�B	��B	�B	��B	�B	�B	��B	��B	�<B	��B	��B	�B	�|B	�%B	�DB	�>B	��B	��B	�TB	�B	�zB	�zB	��B	�FB	��B	��B	�RB	�LB	�fB	�B	��B	�rB	�B	��B	�fB	�lB	�B
'B
KB
1B
fB
zB
B
^B
�B

XB
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
6B
JB
PB
PB
�B
6B
�B
B
�B
�B
�B
JB
^B
0B
^B

�B

XB
	�B
	�B
	�B
	�B

�B
)B
xB
�B
�B
�B
�B
�B
B
~B
0B
JB
~B
dB
�B
�B
pB
B
�B
"B
�B
B
"B
VB
<B
VB
�B
BB
BB
vB
�B
�B
B
}B
4B
&B
�B
2B
�B
B
B
�B
�B
KB
eB
7B
#B
�B
]B
xB
/B
IB
5B
jB
pB
 vB
!bB
!HB
!�B
"�B
"�B
"�B
"�B
"�B
#B
#B
#�B
#nB
#�B
#�B
#�B
#�B
$@B
$tB
$�B
$�B
$@B
$@B
$&B
#�B
"�B
"�B
"�B
#:B
#�B
#�B
$ZB
$ZB
$ZB
$�B
$�B
%FB
%�B
'�B
(�B
)DB
)B
(�B
(�B
(>B
)*B
)yB
*�B
+�B
+kB
+�B
,=B
,�B
,�B
,�B
-�B
-�B
./B
.cB
.}B
.�B
./B
-�B
,�B
,WB
,�B
-CB
,�B
,B
+�B
+QB
+B
+B
*�B
*�B
+kB
+6B
*�B
*�B
*�B
*�B
+�B
-B
-�B
-�B
.IB
.}B
.�B
.�B
.�B
/OB
/�B
/OB
/�B
/�B
/B
.�B
.�B
.�B
.}B
.IB
./B
-�B
-�B
./B
.�B
.cB
-�B
-�B
-�B
.}B
.IB
.�B
/OB
/5B
/�B
/B
.�B
.cB
.cB
.B
./B
-�B
.cB
.}B
.}B
.cB
.cB
.�B
.�B
.�B
/B
/iB
/�B
/�B
/�B
/�B
/�B
0!B
1B
1[B
1vB
2aB
33B
3hB
3�B
3�B
3�B
4B
4TB
4�B
4�B
5B
5�B
5�B
6�B
6`B
6�B
72B
7�B
7fB
6�B
6�B
6�B
7LB
7�B
8RB
8�B
8�B
8�B
9	B
9>B
9>B
9�B
9�B
9�B
:B
;JB
;dB
;�B
<�B
=�B
>�B
?�B
A B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
DMB
D�B
D�B
D�B
D3B
DB
D�B
E�B
F%B
F?B
GB
G_B
G�B
G�B
GzB
G�B
H�B
HKB
H�B
H�B
I�B
J#B
I�B
JXB
J�B
J�B
K�B
LB
MB
L�B
L�B
MPB
N<B
NVB
NVB
N�B
O(B
O\B
O\B
O�B
PHB
P�B
P}B
P�B
P�B
QB
Q4B
QhB
Q�B
R�B
R�B
R�B
R�B
S@B
SuB
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
W$B
W$B
W�B
W�B
W�B
XB
XB
XB
X+B
W�B
W�B
X+B
XB
XEB
X�B
X�B
YB
ZB
ZB
ZB
Y�B
Z7B
[	B
[#B
[qB
[�B
[�B
[�B
\CB
\]B
\xB
\�B
\�B
\�B
]/B
]/B
]/B
]/B
]/B
]/B
]B
^B
^B
^5B
^5B
^B
^OB
^�B
^�B
^�B
^�B
^�B
_pB
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
`'B
`vB
`�B
`�B
`�B
a-B
a|B
abB
a�B
a�B
a�B
bB
bhB
b�B
cB
c�B
c�B
c�B
dB
dB
c�B
c�B
d&B
d�B
eB
eFB
e�B
e�B
fB
f�B
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
i*B
iyB
i_B
i�B
i�B
jB
j�B
kB
k�B
k�B
k�B
l=B
l�B
l�B
mB
m)B
m)B
mCB
m�B
m�B
nB
n�B
o�B
poB
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
sMB
shB
s3B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
vFB
v+B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
x�B
x8B
xRB
xRB
xlB
x�B
x�B
x�B
xlB
xRB
x�B
x�B
x�B
x�B
yXB
yXB
yrB
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{0B
{0B
{0B
{dB
{�B
|6B
|�B
|�B
}B
}B
}"B
}qB
}�B
}�B
~BB
~wB
~wB
~�B
.B
�B
�B
�4B
�B
�4B
�OB
��B
��B
��B
� B
�B
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
��B
��B
�-B
�GB
�aB
��B
�3B
��B
�9B
�SB
�SB
��B
�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	abB	abB	a-B	abB	abB	abB	aB	a-B	aB	aB	aB	`�B	a-B	aB	a-B	aB	aB	aHB	abB	a-B	aB	aB	`�B	aB	`�B	`�B	aB	a�B	cnB	b�B	a�B	��B
B
-CB
5?B
6`B
6�B
7B
7�B
8RB
9>B
:*B
:�B
;JB
<B
=qB
@ B
C�B
AB
O�B
��B
��B
�WB
�&B
��B�B8�BY�B`�Bk�BaHBT{B*�B#B%FB �B�B
�cB
��B
��B
G�B
1�B
1�B
QB	��B	�B	��B	�JB	�B	�B	�kB	�~B	��B	�:B	�6B	y�B	h�B	Y1B	MPB	LJB	KB	I�B	G�B	G�B	J�B	N�B	MPB	J�B	KxB	P�B	K�B	IlB	@�B	K)B	N"B	E�B	>BB	3B	"�B	B	%`B	/B	6�B	<B	=�B	?B	3B	0�B	(sB	(�B	*�B	(�B	&B	"4B	�B	"�B	$tB	6FB	M�B	Y1B	j�B	�4B	�B	�B	�=B	�)B	��B	�RB
�B
B
B
�B

�B
9B
 B	��B	�B	ªB	��B	��B	�"B	��B	�]B	��B	��B	��B	�{B	�AB	�B	~�B	��B	�B	��B	��B	��B	�~B	��B	��B	��B	бB	��B	��B	�B	��B	�2B	�B	�,B	�,B	�bB	��B	�]B	�|B	�bB	�OB	��B	�$B	��B	��B	ɺB	��B	�:B	ѝB	�4B	�.B	οB	�6B	�B	��B	�B	רB	��B	ԕB	ϑB	��B	�B	��B	�jB	��B	��B	ƨB	��B	�iB	��B	�$B	�rB	��B	�FB	�zB	�B	��B	�2B	�8B	��B	�dB	�OB	�B	��B	ÖB	�B	�+B	�EB	��B	�B	ɆB	��B	��B	�#B	�XB	��B	��B	��B	ЗB	��B	бB	��B	�hB	��B	� B	�oB	�B	��B	�B	ӏB	ԯB	�gB	�B	�
B	�sB	�B	��B	�B	�B	�#B	�B	��B	�)B	ܒB	ݘB	��B	ݲB	��B	��B	�jB	��B	��B	��B	��B	��B	�B	�&B	��B	��B	��B	�>B	��B	��B	�)B	�qB	�B	�B	�sB	��B	�B	�CB	�!B	�B	�lB	�B	�nB	��B	��B	�UB	�B	�B	��B	�-B	�B	�	B	�XB	�	B	��B	��B	��B	�JB	�B	�^B	�8B	��B	�fB	�lB	�>B	�rB	��B	��B	�^B	��B	�^B	��B	��B	�	B	��B	��B	��B	�B	��B	�B	��B	�B	�B	��B	��B	�<B	��B	��B	�B	�|B	�%B	�DB	�>B	��B	��B	�TB	�B	�zB	�zB	��B	�FB	��B	��B	�RB	�LB	�fB	�B	��B	�rB	�B	��B	�fB	�lB	�B
'B
KB
1B
fB
zB
B
^B
�B

XB
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
6B
JB
PB
PB
�B
6B
�B
B
�B
�B
�B
JB
^B
0B
^B

�B

XB
	�B
	�B
	�B
	�B

�B
)B
xB
�B
�B
�B
�B
�B
B
~B
0B
JB
~B
dB
�B
�B
pB
B
�B
"B
�B
B
"B
VB
<B
VB
�B
BB
BB
vB
�B
�B
B
}B
4B
&B
�B
2B
�B
B
B
�B
�B
KB
eB
7B
#B
�B
]B
xB
/B
IB
5B
jB
pB
 vB
!bB
!HB
!�B
"�B
"�B
"�B
"�B
"�B
#B
#B
#�B
#nB
#�B
#�B
#�B
#�B
$@B
$tB
$�B
$�B
$@B
$@B
$&B
#�B
"�B
"�B
"�B
#:B
#�B
#�B
$ZB
$ZB
$ZB
$�B
$�B
%FB
%�B
'�B
(�B
)DB
)B
(�B
(�B
(>B
)*B
)yB
*�B
+�B
+kB
+�B
,=B
,�B
,�B
,�B
-�B
-�B
./B
.cB
.}B
.�B
./B
-�B
,�B
,WB
,�B
-CB
,�B
,B
+�B
+QB
+B
+B
*�B
*�B
+kB
+6B
*�B
*�B
*�B
*�B
+�B
-B
-�B
-�B
.IB
.}B
.�B
.�B
.�B
/OB
/�B
/OB
/�B
/�B
/B
.�B
.�B
.�B
.}B
.IB
./B
-�B
-�B
./B
.�B
.cB
-�B
-�B
-�B
.}B
.IB
.�B
/OB
/5B
/�B
/B
.�B
.cB
.cB
.B
./B
-�B
.cB
.}B
.}B
.cB
.cB
.�B
.�B
.�B
/B
/iB
/�B
/�B
/�B
/�B
/�B
0!B
1B
1[B
1vB
2aB
33B
3hB
3�B
3�B
3�B
4B
4TB
4�B
4�B
5B
5�B
5�B
6�B
6`B
6�B
72B
7�B
7fB
6�B
6�B
6�B
7LB
7�B
8RB
8�B
8�B
8�B
9	B
9>B
9>B
9�B
9�B
9�B
:B
;JB
;dB
;�B
<�B
=�B
>�B
?�B
A B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
DMB
D�B
D�B
D�B
D3B
DB
D�B
E�B
F%B
F?B
GB
G_B
G�B
G�B
GzB
G�B
H�B
HKB
H�B
H�B
I�B
J#B
I�B
JXB
J�B
J�B
K�B
LB
MB
L�B
L�B
MPB
N<B
NVB
NVB
N�B
O(B
O\B
O\B
O�B
PHB
P�B
P}B
P�B
P�B
QB
Q4B
QhB
Q�B
R�B
R�B
R�B
R�B
S@B
SuB
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
V�B
W$B
W$B
W�B
W�B
W�B
XB
XB
XB
X+B
W�B
W�B
X+B
XB
XEB
X�B
X�B
YB
ZB
ZB
ZB
Y�B
Z7B
[	B
[#B
[qB
[�B
[�B
[�B
\CB
\]B
\xB
\�B
\�B
\�B
]/B
]/B
]/B
]/B
]/B
]/B
]B
^B
^B
^5B
^5B
^B
^OB
^�B
^�B
^�B
^�B
^�B
_pB
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
`'B
`vB
`�B
`�B
`�B
a-B
a|B
abB
a�B
a�B
a�B
bB
bhB
b�B
cB
c�B
c�B
c�B
dB
dB
c�B
c�B
d&B
d�B
eB
eFB
e�B
e�B
fB
f�B
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
i*B
iyB
i_B
i�B
i�B
jB
j�B
kB
k�B
k�B
k�B
l=B
l�B
l�B
mB
m)B
m)B
mCB
m�B
m�B
nB
n�B
o�B
poB
p�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
sMB
shB
s3B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
vzB
v�B
vFB
v+B
v+B
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
x�B
x8B
xRB
xRB
xlB
x�B
x�B
x�B
xlB
xRB
x�B
x�B
x�B
x�B
yXB
yXB
yrB
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{0B
{0B
{0B
{dB
{�B
|6B
|�B
|�B
}B
}B
}"B
}qB
}�B
}�B
~BB
~wB
~wB
~�B
.B
�B
�B
�4B
�B
�4B
�OB
��B
��B
��B
� B
�B
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
��B
��B
�-B
�GB
�aB
��B
�3B
��B
�9B
�SB
�SB
��B
�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230212184356  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230212184358  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230212184359  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230212184400                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230212184400  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230212184400  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230212185918                      G�O�G�O�G�O�                