CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:20:36Z creation;2022-06-04T19:20:36Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192036  20230309114502  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�4�k�5�1   @�4��vT2@-�hr�!�cf�1'1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�33B���B���B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B���B���B�  B�  B�  B���B���B�33B���C  C  C  C  C
  C33C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,L�C.33C/��C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @
>@}p�@��R@��RA\)A?\)A_\)A\)A��GA��A��A��AϮA߮A�A��B�
B�
B=pB�
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
B��B��B��B��B��B��B�Q�B��B��B��B��B��RB��RB��B��B��B�Q�BǸRB˸RB��B��B��B��B߸RB�RB��B��B��B��RB��B��B��RC��C��C��C��C	��C(�C��C��C�)C��C��C��C��C��C��C��C!��C#��C%��C'��C*]C,B�C.(�C/C1�)C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ]C[��C]�)C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�D���D���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�hR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A���A���A���A���A���A��rA��xA��A��rA��A��	A��>A��"A��A���A���A��]A��rA��	A��8A��>A���A���A���A��cA���A��A��Aח�A�@�A֨XA�xlA��AӺ*AӁ;A��A�ѷAπ4AΤA���A�_�A�^�A�~�AŃ�A�MA��%A��A���A�W�A�GEA�$�A���A���A�J#A��fA�2aA���A���A�($A��BA�5?A�[#A��.A��A���A��A��7A�EA��9A��A���A���A�8�A��A�҉A�9�A���A��A��YA��YA��!A���A�خA��BA��A���A}�YAp�Ak�Aj�eAiK�AgHAe��AdcAb��A`A]�MA[�jAW��AUϫASیAO�|AK�EAHخAG�AD�AA��A?�A>��A=�A;C�A8!A6�VA4SA0��A.g8A-�$A,�<A+��A*�A'��A'�A&��A%!-A$xlA#��A#�A"��A"+kA!��A N<AsA�YAMA(A &A �A A!��A!�.A!�A!PHA �A ��A �A � A �'A!�A!�ZA!�eA!hsA"Z�A ںA �aA �EA �gA �jA!	lA �'A ��A w2A #�AS�AĜA.�A�A��A�\A�AѷAr�A$�A�Ab�A�AݘA��As�A��A��A�rAg8A�}AVA��A/�A��AS&A�A�vA�nA\�A�A
�,A
�{A
5�A	V�A	�AѷAe�A/�A�A�AA�A��A iA$tA��AOvA�.A$tA�Ae�A2aA�zA�AA �[A iD@��@�e,@�g�@���@�($@�8�@��#@���@��@�8�@�w�@��@@�[�@��f@��@�z@�x@��@�Y@�[�@�� @�g�@�!-@�@��@�4�@�:*@�H@�Y�@�A�@��@�c�@��q@�)�@��@�A @��U@���@�֡@�z@�x@�@@�*0@�@�]d@�]�@䭬@�3�@�@�K�@���@�$@�[�@�p�@��s@�p;@��@�A @ުe@��@�z@�A @��@�.�@���@�>�@��@�+k@݄M@��@ܚ@�g8@�_�@��K@�~(@�z@�{�@��@�u�@��E@�z�@�/�@מ�@�@O@֗�@��@��@հ�@�dZ@�9X@��@�r�@�(�@Б�@�-�@�;@��E@Ξ@�5?@͸�@�H�@�@��@��X@�R�@��@�~@�B[@�c @��@˲-@���@ɴ�@�G�@�m]@��r@��T@�S&@��@�#�@�q@Ȱ!@ș1@�^5@���@�S�@��@Ƴh@�˒@�$t@��f@�ȴ@��z@×$@õt@���@�ݘ@�X@�ѷ@�oi@��
@�Vm@�1�@��@���@�[�@�?@��o@�\�@�#�@�Q�@�T�@�d�@��@��-@��r@��@��"@�u%@��^@�o@��s@��@��@�Mj@��@�/�@���@�a|@���@�X@��@���@�)�@���@�A @��@�9�@��@���@�oi@��@��@�ی@�R�@�O@��w@�F�@��@��'@��@�&@��@���@�K^@��@���@���@��C@��f@�c@�S&@�+@���@�Z@�7�@���@��n@��C@��:@�?}@���@��?@���@��@�ں@��b@�B[@��m@���@�K�@�+@���@��<@���@�1�@��@��g@���@��@��M@��,@���@��Z@��N@��@�s�@���@�r�@���@��@�(�@��@���@�z�@�6�@��g@���@�F@���@�PH@��
@�k�@�@�U2@��K@�5�@��/@��@�c�@�)�@���@���@�a@�
=@��U@�e�@�3�@��F@�x�@�=�@��X@�}V@�,=@���@���@���@��/@�y>@�O@��d@��:@�hs@�q@��@���@��\@�h
@�Z@�b@��o@���@�W?@�Ĝ@�.�@�	@��[@��7@�q@�r�@�Ta@�D�@�-�@�4@��-@�c�@�A @� \@��@��@���@�($@��@���@���@���@�A�@��c@���@���@��x@���@�^5@�-�@��}@�x@�b�@�Y�@�J#@�5�@�"�@��@��y@���@��6@���@�Ft@�	@���@��h@��M@�a�@�(@��@��<@�z�@�A�@��o@��g@���@�s@�G�@�[W@�_p@��@�m�@��@��@���@��@��}@���@��:@��@���@��@���@��\@��@~��@~u@}ԕ@}��@}<6@}q@|�[@|_@|~@{�W@{��@{Z�@{�@z��@z=q@y+�@x�Y@xg8@x4n@xx@w��@v�2@v��@vL0@v�@u�@uY�@u+�@t�$@t/�@s��@r�]@rR�@rE�@r�@q@q+�@p��@p��@p��@p�Y@pr�@p%�@pG@o�&@o��@o��@o�@n҉@nq�@n�@ma�@l��@l��@lg8@k˒@kj�@j�"@j� @j{@i��@i�M@i|@ij@i�@h�.@hM@g�@g�@f�1@fOv@e��@e��@eVm@e;@d�$@d7@c��@c�@@cg�@c4�@b�M@b�1@b�@a�@a=�@a%F@a#�@`��@`��@`q@`1'@_��@^Ta@]ԕ@]�"@]IR@]@@\�@[�]@[�@Zh
@Z)�@Y��@Y�z@Y�^@Y��@Y#�@X�u@X�@WdZ@V~�@V$�@U��@Uzx@UrG@UY�@U*0@TV�@S��@S�@R��@R($@Q�)@Q�z@Q�7@Qx�@Qe,@Q`B@Q[W@QT�@Q4@PɆ@PD�@O�@OA�@N�@N�r@N#:@M0�@L��@L�@Kخ@K��@K��@K�4@K�@K(@J�B@J�1@JC�@I�d@IO�@H�?@H�u@Hu�@HPH@G��@G�w@G�@@G��@G&@F�@Fs�@FW�@E�@E�=@ET�@E�@D��@D�u@D_@D�@C˒@C�*@C�4@CO@C+@B�c@B��@BV@A�)@A��@AIR@@��@@�|@@��@@Ft@?�0@?8@>�@>
�@=��@=�7@=�@<�.@<4n@;�@;�q@;v`@;C�@;!-@:�@:�@:L0@:@9�@9�H@9[W@8�|@8��@8��@8�@8`�@8U2@86@8�@7�@@7!-@7@6�c@6�@6p;@5�#@5rG@5?}@4�@4c�@3��@3x@3P�@3�@2�@2@�@1�D@1�'@1x�@0�`@0|�@0H@/��@/�w@/��@/��@/=@/"�@/�@.�H@.��@.��@.s�@.$�@.@-�o@-�-@-=�@,�e@,S�@+�+@+x@+;d@+�@*�x@*Z�@)��@)�@)�@)�@)J�@(�@(��@(��@(?�@'�@'��@'4�@&��@&� @&R�@&@�@&($@%��@%|@%s�@%k�@%\�@$��@$�e@$tT@$:�@$7@$�@#�@#ݘ@#��@#�F@#�$@#K�@#�@"�<@"�r@"R�@"=q@"!�@!�@!�N@!�~@!B�@!<6@!-w@!*0@! \@!�@ ��@ �@�@�@a@@�'@h
@.�@�t@q@��@�@]d@�;@��@�:@t�@W?@�@�@��@�'@�@�\@_�@u@��@�C@[W@F@*0@��@K^@˒@��@�f@j�@J#@C�@=@.I@�@�@�8@�2@�@{�@W�@)�@�z@�"@�@�I@'R@1@�q@�:@�{@S�@�8@�}@��@��@z@=q@.�@4@��@�@�^@Q�@�@��@��@�@w�@Xy@7�@�r@�;@�
@˒@��@��@�P@dZ@F�@)_@�@͟@��@H�@�@�@��@�d@��@j@F@ \@�@�?@�@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A���A���A���A���A���A��rA��xA��A��rA��A��	A��>A��"A��A���A���A��]A��rA��	A��8A��>A���A���A���A��cA���A��A��Aח�A�@�A֨XA�xlA��AӺ*AӁ;A��A�ѷAπ4AΤA���A�_�A�^�A�~�AŃ�A�MA��%A��A���A�W�A�GEA�$�A���A���A�J#A��fA�2aA���A���A�($A��BA�5?A�[#A��.A��A���A��A��7A�EA��9A��A���A���A�8�A��A�҉A�9�A���A��A��YA��YA��!A���A�خA��BA��A���A}�YAp�Ak�Aj�eAiK�AgHAe��AdcAb��A`A]�MA[�jAW��AUϫASیAO�|AK�EAHخAG�AD�AA��A?�A>��A=�A;C�A8!A6�VA4SA0��A.g8A-�$A,�<A+��A*�A'��A'�A&��A%!-A$xlA#��A#�A"��A"+kA!��A N<AsA�YAMA(A &A �A A!��A!�.A!�A!PHA �A ��A �A � A �'A!�A!�ZA!�eA!hsA"Z�A ںA �aA �EA �gA �jA!	lA �'A ��A w2A #�AS�AĜA.�A�A��A�\A�AѷAr�A$�A�Ab�A�AݘA��As�A��A��A�rAg8A�}AVA��A/�A��AS&A�A�vA�nA\�A�A
�,A
�{A
5�A	V�A	�AѷAe�A/�A�A�AA�A��A iA$tA��AOvA�.A$tA�Ae�A2aA�zA�AA �[A iD@��@�e,@�g�@���@�($@�8�@��#@���@��@�8�@�w�@��@@�[�@��f@��@�z@�x@��@�Y@�[�@�� @�g�@�!-@�@��@�4�@�:*@�H@�Y�@�A�@��@�c�@��q@�)�@��@�A @��U@���@�֡@�z@�x@�@@�*0@�@�]d@�]�@䭬@�3�@�@�K�@���@�$@�[�@�p�@��s@�p;@��@�A @ުe@��@�z@�A @��@�.�@���@�>�@��@�+k@݄M@��@ܚ@�g8@�_�@��K@�~(@�z@�{�@��@�u�@��E@�z�@�/�@מ�@�@O@֗�@��@��@հ�@�dZ@�9X@��@�r�@�(�@Б�@�-�@�;@��E@Ξ@�5?@͸�@�H�@�@��@��X@�R�@��@�~@�B[@�c @��@˲-@���@ɴ�@�G�@�m]@��r@��T@�S&@��@�#�@�q@Ȱ!@ș1@�^5@���@�S�@��@Ƴh@�˒@�$t@��f@�ȴ@��z@×$@õt@���@�ݘ@�X@�ѷ@�oi@��
@�Vm@�1�@��@���@�[�@�?@��o@�\�@�#�@�Q�@�T�@�d�@��@��-@��r@��@��"@�u%@��^@�o@��s@��@��@�Mj@��@�/�@���@�a|@���@�X@��@���@�)�@���@�A @��@�9�@��@���@�oi@��@��@�ی@�R�@�O@��w@�F�@��@��'@��@�&@��@���@�K^@��@���@���@��C@��f@�c@�S&@�+@���@�Z@�7�@���@��n@��C@��:@�?}@���@��?@���@��@�ں@��b@�B[@��m@���@�K�@�+@���@��<@���@�1�@��@��g@���@��@��M@��,@���@��Z@��N@��@�s�@���@�r�@���@��@�(�@��@���@�z�@�6�@��g@���@�F@���@�PH@��
@�k�@�@�U2@��K@�5�@��/@��@�c�@�)�@���@���@�a@�
=@��U@�e�@�3�@��F@�x�@�=�@��X@�}V@�,=@���@���@���@��/@�y>@�O@��d@��:@�hs@�q@��@���@��\@�h
@�Z@�b@��o@���@�W?@�Ĝ@�.�@�	@��[@��7@�q@�r�@�Ta@�D�@�-�@�4@��-@�c�@�A @� \@��@��@���@�($@��@���@���@���@�A�@��c@���@���@��x@���@�^5@�-�@��}@�x@�b�@�Y�@�J#@�5�@�"�@��@��y@���@��6@���@�Ft@�	@���@��h@��M@�a�@�(@��@��<@�z�@�A�@��o@��g@���@�s@�G�@�[W@�_p@��@�m�@��@��@���@��@��}@���@��:@��@���@��@���@��\@��@~��@~u@}ԕ@}��@}<6@}q@|�[@|_@|~@{�W@{��@{Z�@{�@z��@z=q@y+�@x�Y@xg8@x4n@xx@w��@v�2@v��@vL0@v�@u�@uY�@u+�@t�$@t/�@s��@r�]@rR�@rE�@r�@q@q+�@p��@p��@p��@p�Y@pr�@p%�@pG@o�&@o��@o��@o�@n҉@nq�@n�@ma�@l��@l��@lg8@k˒@kj�@j�"@j� @j{@i��@i�M@i|@ij@i�@h�.@hM@g�@g�@f�1@fOv@e��@e��@eVm@e;@d�$@d7@c��@c�@@cg�@c4�@b�M@b�1@b�@a�@a=�@a%F@a#�@`��@`��@`q@`1'@_��@^Ta@]ԕ@]�"@]IR@]@@\�@[�]@[�@Zh
@Z)�@Y��@Y�z@Y�^@Y��@Y#�@X�u@X�@WdZ@V~�@V$�@U��@Uzx@UrG@UY�@U*0@TV�@S��@S�@R��@R($@Q�)@Q�z@Q�7@Qx�@Qe,@Q`B@Q[W@QT�@Q4@PɆ@PD�@O�@OA�@N�@N�r@N#:@M0�@L��@L�@Kخ@K��@K��@K�4@K�@K(@J�B@J�1@JC�@I�d@IO�@H�?@H�u@Hu�@HPH@G��@G�w@G�@@G��@G&@F�@Fs�@FW�@E�@E�=@ET�@E�@D��@D�u@D_@D�@C˒@C�*@C�4@CO@C+@B�c@B��@BV@A�)@A��@AIR@@��@@�|@@��@@Ft@?�0@?8@>�@>
�@=��@=�7@=�@<�.@<4n@;�@;�q@;v`@;C�@;!-@:�@:�@:L0@:@9�@9�H@9[W@8�|@8��@8��@8�@8`�@8U2@86@8�@7�@@7!-@7@6�c@6�@6p;@5�#@5rG@5?}@4�@4c�@3��@3x@3P�@3�@2�@2@�@1�D@1�'@1x�@0�`@0|�@0H@/��@/�w@/��@/��@/=@/"�@/�@.�H@.��@.��@.s�@.$�@.@-�o@-�-@-=�@,�e@,S�@+�+@+x@+;d@+�@*�x@*Z�@)��@)�@)�@)�@)J�@(�@(��@(��@(?�@'�@'��@'4�@&��@&� @&R�@&@�@&($@%��@%|@%s�@%k�@%\�@$��@$�e@$tT@$:�@$7@$�@#�@#ݘ@#��@#�F@#�$@#K�@#�@"�<@"�r@"R�@"=q@"!�@!�@!�N@!�~@!B�@!<6@!-w@!*0@! \@!�@ ��@ �@�@�@a@@�'@h
@.�@�t@q@��@�@]d@�;@��@�:@t�@W?@�@�@��@�'@�@�\@_�@u@��@�C@[W@F@*0@��@K^@˒@��@�f@j�@J#@C�@=@.I@�@�@�8@�2@�@{�@W�@)�@�z@�"@�@�I@'R@1@�q@�:@�{@S�@�8@�}@��@��@z@=q@.�@4@��@�@�^@Q�@�@��@��@�@w�@Xy@7�@�r@�;@�
@˒@��@��@�P@dZ@F�@)_@�@͟@��@H�@�@�@��@�d@��@j@F@ \@�@�?@�@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�2B	�2B	�LB	�fB	�B	�2B	�fB	�LB	�LB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�dB	�B	�XB	��B	��B	��B	�/B	�&B	�B	��B	.B	u�B	shB	qvB	oOB	j0B	��B	�B	��B	ݘB	��B	�B	��B	�TB	�B	�nB
9B
[B
=B
QhB
\]B
�hB
�B
�B
�aB
�B�B<�B?�B3�B+B�BA BkkBW�BFtB5B&�B�BoBB
�PB
�B
�tB
��B
h�B
FYB
-�B	��B	�
B	v�B	pUB	i�B	`�B	[qB	T�B	L�B	?HB	6zB	.cB	#�B	�B	B	�B	 iB��B��B�JB��B�'B�B�B�|B��BٴB�B�uB��BӏB�SBخB�B�B�LB��B��B�'BݲB�|B�B�B	B	�B	~B	�B	�B	0�B	U�B	uZB	~]B	��B	�eB	�IB	�=B	�B	��B	��B	�(B	� B	��B	��B	�CB	�MB
B
(B
�B
-�B
6�B
="B
DMB
DgB
DMB
C�B
@�B
2�B
&2B
"�B
%�B
,qB
/�B
,�B
)�B
$�B
pB	��B	�wB	�
B	�!B	��B	�uB	�"B	ȀB	�-B	��B	�-B	ĶB	�zB	ɺB	�xB	�B	�B	�pB	�(B	��B	�HB	��B	�4B	�B	�&B	�@B	ӏB	�B	��B	��B	�mB	�+B	��B	��B	��B	�B	ؓB	�B	�5B	�-B	�|B	�B	��B	��B	��B	ܒB	چB	ڠB	�xB	�CB	��B	��B	��B	�VB	ߊB	�vB	�|B	�B	�B	�HB	�B	�vB	��B	�'B	ߊB	�pB	�VB	�!B	��B	�;B	��B	ބB	�B	��B	޸B	�B	ߊB	߾B	ߊB	�B	ޞB	�jB	�B	�	B	�KB	�_B	��B	�EB	��B	ۦB	ܒB	�B	��B	��B	�B	��B	�	B	ڠB	�eB	ּB	��B	�2B	�B	�MB	�SB	�-B	�kB	��B	��B	�B	��B	�B	�GB	�vB	��B	�'B	�'B	�B	�'B	��B	�UB	��B	�B	��B	�hB	��B	�B	�hB	�B	�MB	�aB	�-B	�B	��B	��B	�B	�/B	�WB	�6B	�*B	��B	�B	�B	�B	�B	��B	��B	�DB	�B	�_B	��B	��B	�GB	��B	��B	�*B	�DB	��B	��B	�B	�B
 �B
{B
�B
AB
�B
�B
�B
B
�B
B
mB
YB
�B
B
�B
 �B
'B
AB
B
�B
EB
	�B
�B
�B
�B
fB
	B
	B
	�B

	B

	B

XB

rB
	lB
	7B
�B
B
oB
oB
-B	�cB	�<B	��B	��B	��B	��B	��B
�B
�B
�B
[B
�B	�}B	��B	�B	�	B	�lB	�*B	��B	�XB	��B	��B	�6B	�6B	�B	��B	��B	�<B	��B
  B
 OB
 OB
oB
;B
UB
�B
�B
oB
 �B
 4B
 B
 B
 B
 OB
 �B
 �B
 iB
�B
�B
�B
�B
B
gB
B
9B
B
�B
�B
�B
�B
KB
�B
�B
�B
	�B
0B
6B
jB
�B
�B
VB
pB
pB
�B
�B
�B
�B
�B
�B
 B
�B
NB
hB
�B
B
HB
�B
�B
NB
 B
�B
�B
 B
�B
�B
B
aB
�B
B
MB
�B
MB
�B
�B
�B
�B
�B
2B
B
�B
�B
B
2B
�B
�B
�B
�B
$B
?B
�B
sB
sB
�B
�B
1B
KB
�B
�B
�B
�B
	B
=B
�B
B
B
IB
�B
B
OB
B
B
~B
IB
dB
�B
�B
�B
�B
�B
�B
�B
pB
 B
�B
�B
 BB
 �B
 �B
!-B
!|B
!bB
!�B
"NB
"�B
"�B
#B
#:B
#TB
#�B
$&B
$�B
$�B
$�B
$�B
%B
%,B
%FB
%zB
%�B
&B
&B
&�B
&�B
'RB
'�B
'�B
(
B
(XB
(�B
)�B
)�B
*KB
*�B
*B
*�B
+kB
+�B
,B
-B
-wB
/OB
0UB
0oB
0oB
0UB
0;B
0!B
/�B
0�B
0�B
1[B
2|B
2GB
1�B
1'B
1[B
1[B
1�B
1�B
1�B
2B
2�B
2�B
2�B
3B
3hB
3�B
3�B
4B
5tB
6`B
6zB
6�B
6�B
7fB
7�B
7�B
7�B
8B
9	B
9XB
9XB
9�B
9�B
:B
:xB
:�B
:�B
:�B
:�B
;dB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<B
<B
<jB
<PB
<jB
<�B
=VB
=<B
=VB
=�B
=�B
>(B
>]B
>wB
>�B
?HB
?HB
?HB
?.B
?cB
?�B
@4B
@4B
@�B
AUB
AoB
A�B
B'B
B'B
BAB
B[B
CGB
CB
CGB
C�B
C{B
C�B
C�B
DgB
D�B
EB
D�B
D�B
E9B
ESB
ESB
E9B
E�B
G+B
G+B
GzB
G�B
G�B
G�B
HfB
HfB
HKB
HKB
HB
H�B
HfB
H�B
IB
I�B
I�B
J=B
J�B
J�B
KB
K)B
KDB
KDB
K^B
K�B
L0B
L�B
M6B
M�B
NB
NVB
NpB
NpB
N�B
NpB
N�B
NpB
NpB
N�B
O(B
O�B
O�B
O�B
P.B
PHB
Q4B
QhB
Q�B
RB
Q�B
Q�B
RB
R:B
R:B
RTB
R:B
RoB
R�B
R�B
SB
SB
SB
S&B
S[B
S�B
S�B
S�B
S�B
S�B
TFB
S�B
T�B
T�B
UB
UMB
UMB
UgB
U�B
U�B
VB
VB
VSB
VSB
VmB
V�B
V�B
W
B
W?B
W?B
W�B
W�B
W�B
XB
X+B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[	B
[WB
[qB
[�B
[�B
\)B
\B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_;B
_pB
_�B
`'B
`B
`\B
`vB
`�B
`�B
aHB
a-B
a�B
bB
b4B
b�B
b�B
b�B
b�B
cB
c B
cTB
c:B
cTB
cnB
cnB
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
f2B
fB
f�B
f�B
f�B
gB
f�B
gB
gmB
g�B
h
B
h>B
h>B
h�B
h�B
h�B
h�B
i*B
i�B
iyB
i�B
iyB
i�B
j0B
jKB
jB
jB
j�B
j�B
j�B
kB
j�B
kB
k6B
kQB
k�B
k�B
k�B
k�B
l"B
lWB
l"B
l�B
m)B
mB
m)B
l�B
m)B
mB
m]B
m�B
m�B
n/B
n�B
n�B
o5B
o�B
o�B
pB
pUB
p�B
p�B
p�B
qB
qAB
q'B
qAB
q'B
q�B
q�B
q�B
q�B
rB
q�B
rB
r|B
r�B
r�B
sB
sB
r�B
sMB
s�B
tTB
t9B
tTB
t9B
t9B
t9B
t9B
tB
t9B
t9B
tTB
t9B
t�B
t�B
t�B
t�B
u%B
uB
u�B
vFB
v�B
v�B
wLB
wLB
w2B
wLB
w�B
w�B
w�B
w�B
xB
xlB
x8B
xlB
x�B
x�B
x�B
y	B
y>B
y>B
yXB
y�B
y�B
y�B
zB
z*B
z^B
zDB
zDB
zDB
z^B
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
|B
|B
|6B
|6B
|�B
|�B
|�B
}B
}"B
}<B
}"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�2B	�2B	�LB	�fB	�B	�2B	�fB	�LB	�LB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�dB	�B	�XB	��B	��B	��B	�/B	�&B	�B	��B	.B	u�B	shB	qvB	oOB	j0B	��B	�B	��B	ݘB	��B	�B	��B	�TB	�B	�nB
9B
[B
=B
QhB
\]B
�hB
�B
�B
�aB
�B�B<�B?�B3�B+B�BA BkkBW�BFtB5B&�B�BoBB
�PB
�B
�tB
��B
h�B
FYB
-�B	��B	�
B	v�B	pUB	i�B	`�B	[qB	T�B	L�B	?HB	6zB	.cB	#�B	�B	B	�B	 iB��B��B�JB��B�'B�B�B�|B��BٴB�B�uB��BӏB�SBخB�B�B�LB��B��B�'BݲB�|B�B�B	B	�B	~B	�B	�B	0�B	U�B	uZB	~]B	��B	�eB	�IB	�=B	�B	��B	��B	�(B	� B	��B	��B	�CB	�MB
B
(B
�B
-�B
6�B
="B
DMB
DgB
DMB
C�B
@�B
2�B
&2B
"�B
%�B
,qB
/�B
,�B
)�B
$�B
pB	��B	�wB	�
B	�!B	��B	�uB	�"B	ȀB	�-B	��B	�-B	ĶB	�zB	ɺB	�xB	�B	�B	�pB	�(B	��B	�HB	��B	�4B	�B	�&B	�@B	ӏB	�B	��B	��B	�mB	�+B	��B	��B	��B	�B	ؓB	�B	�5B	�-B	�|B	�B	��B	��B	��B	ܒB	چB	ڠB	�xB	�CB	��B	��B	��B	�VB	ߊB	�vB	�|B	�B	�B	�HB	�B	�vB	��B	�'B	ߊB	�pB	�VB	�!B	��B	�;B	��B	ބB	�B	��B	޸B	�B	ߊB	߾B	ߊB	�B	ޞB	�jB	�B	�	B	�KB	�_B	��B	�EB	��B	ۦB	ܒB	�B	��B	��B	�B	��B	�	B	ڠB	�eB	ּB	��B	�2B	�B	�MB	�SB	�-B	�kB	��B	��B	�B	��B	�B	�GB	�vB	��B	�'B	�'B	�B	�'B	��B	�UB	��B	�B	��B	�hB	��B	�B	�hB	�B	�MB	�aB	�-B	�B	��B	��B	�B	�/B	�WB	�6B	�*B	��B	�B	�B	�B	�B	��B	��B	�DB	�B	�_B	��B	��B	�GB	��B	��B	�*B	�DB	��B	��B	�B	�B
 �B
{B
�B
AB
�B
�B
�B
B
�B
B
mB
YB
�B
B
�B
 �B
'B
AB
B
�B
EB
	�B
�B
�B
�B
fB
	B
	B
	�B

	B

	B

XB

rB
	lB
	7B
�B
B
oB
oB
-B	�cB	�<B	��B	��B	��B	��B	��B
�B
�B
�B
[B
�B	�}B	��B	�B	�	B	�lB	�*B	��B	�XB	��B	��B	�6B	�6B	�B	��B	��B	�<B	��B
  B
 OB
 OB
oB
;B
UB
�B
�B
oB
 �B
 4B
 B
 B
 B
 OB
 �B
 �B
 iB
�B
�B
�B
�B
B
gB
B
9B
B
�B
�B
�B
�B
KB
�B
�B
�B
	�B
0B
6B
jB
�B
�B
VB
pB
pB
�B
�B
�B
�B
�B
�B
 B
�B
NB
hB
�B
B
HB
�B
�B
NB
 B
�B
�B
 B
�B
�B
B
aB
�B
B
MB
�B
MB
�B
�B
�B
�B
�B
2B
B
�B
�B
B
2B
�B
�B
�B
�B
$B
?B
�B
sB
sB
�B
�B
1B
KB
�B
�B
�B
�B
	B
=B
�B
B
B
IB
�B
B
OB
B
B
~B
IB
dB
�B
�B
�B
�B
�B
�B
�B
pB
 B
�B
�B
 BB
 �B
 �B
!-B
!|B
!bB
!�B
"NB
"�B
"�B
#B
#:B
#TB
#�B
$&B
$�B
$�B
$�B
$�B
%B
%,B
%FB
%zB
%�B
&B
&B
&�B
&�B
'RB
'�B
'�B
(
B
(XB
(�B
)�B
)�B
*KB
*�B
*B
*�B
+kB
+�B
,B
-B
-wB
/OB
0UB
0oB
0oB
0UB
0;B
0!B
/�B
0�B
0�B
1[B
2|B
2GB
1�B
1'B
1[B
1[B
1�B
1�B
1�B
2B
2�B
2�B
2�B
3B
3hB
3�B
3�B
4B
5tB
6`B
6zB
6�B
6�B
7fB
7�B
7�B
7�B
8B
9	B
9XB
9XB
9�B
9�B
:B
:xB
:�B
:�B
:�B
:�B
;dB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<B
<B
<jB
<PB
<jB
<�B
=VB
=<B
=VB
=�B
=�B
>(B
>]B
>wB
>�B
?HB
?HB
?HB
?.B
?cB
?�B
@4B
@4B
@�B
AUB
AoB
A�B
B'B
B'B
BAB
B[B
CGB
CB
CGB
C�B
C{B
C�B
C�B
DgB
D�B
EB
D�B
D�B
E9B
ESB
ESB
E9B
E�B
G+B
G+B
GzB
G�B
G�B
G�B
HfB
HfB
HKB
HKB
HB
H�B
HfB
H�B
IB
I�B
I�B
J=B
J�B
J�B
KB
K)B
KDB
KDB
K^B
K�B
L0B
L�B
M6B
M�B
NB
NVB
NpB
NpB
N�B
NpB
N�B
NpB
NpB
N�B
O(B
O�B
O�B
O�B
P.B
PHB
Q4B
QhB
Q�B
RB
Q�B
Q�B
RB
R:B
R:B
RTB
R:B
RoB
R�B
R�B
SB
SB
SB
S&B
S[B
S�B
S�B
S�B
S�B
S�B
TFB
S�B
T�B
T�B
UB
UMB
UMB
UgB
U�B
U�B
VB
VB
VSB
VSB
VmB
V�B
V�B
W
B
W?B
W?B
W�B
W�B
W�B
XB
X+B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[	B
[WB
[qB
[�B
[�B
\)B
\B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_;B
_pB
_�B
`'B
`B
`\B
`vB
`�B
`�B
aHB
a-B
a�B
bB
b4B
b�B
b�B
b�B
b�B
cB
c B
cTB
c:B
cTB
cnB
cnB
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
f2B
fB
f�B
f�B
f�B
gB
f�B
gB
gmB
g�B
h
B
h>B
h>B
h�B
h�B
h�B
h�B
i*B
i�B
iyB
i�B
iyB
i�B
j0B
jKB
jB
jB
j�B
j�B
j�B
kB
j�B
kB
k6B
kQB
k�B
k�B
k�B
k�B
l"B
lWB
l"B
l�B
m)B
mB
m)B
l�B
m)B
mB
m]B
m�B
m�B
n/B
n�B
n�B
o5B
o�B
o�B
pB
pUB
p�B
p�B
p�B
qB
qAB
q'B
qAB
q'B
q�B
q�B
q�B
q�B
rB
q�B
rB
r|B
r�B
r�B
sB
sB
r�B
sMB
s�B
tTB
t9B
tTB
t9B
t9B
t9B
t9B
tB
t9B
t9B
tTB
t9B
t�B
t�B
t�B
t�B
u%B
uB
u�B
vFB
v�B
v�B
wLB
wLB
w2B
wLB
w�B
w�B
w�B
w�B
xB
xlB
x8B
xlB
x�B
x�B
x�B
y	B
y>B
y>B
yXB
y�B
y�B
y�B
zB
z*B
z^B
zDB
zDB
zDB
z^B
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
|B
|B
|6B
|6B
|�B
|�B
|�B
}B
}"B
}<B
}"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192036  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192036  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192036                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042046  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042046  QCF$                G�O�G�O�G�O�             100JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                JA  ARFMdecpA30a                                                                20230307235410  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230308001538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230308001539  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230308001539                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230308001539  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230308001539  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230309114502                      G�O�G�O�G�O�                