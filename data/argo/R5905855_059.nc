CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:20:56Z creation;2022-06-04T19:20:56Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604192056  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ;A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�9���1   @�9����@-�;dZ��cp�hr�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB���B���B�  B�  B�  B�  B���B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  B�  B�  C   C  C�3C�3C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,L�C-�fC0  C2�C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^33C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�<�Dǀ D�� D�  D�@ Dȃ3D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @
>@}p�@��@��RA\)A?\)A_\)A�z�A��A��A��A��AϮA߮A�A��B�
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
B�Q�B��RB��RB��B��B��B��B��B��RB��B��RB��B��B��B��B��B��B��B��B��B��B��B۸RB��B�RB��B��B��B��B��B��B��C��C��C��C�)C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C,B�C-�)C/��C2]C3��C5��C7�)C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C^(�C_��Ca��Cc��Ce�)Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDz�Dz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�;�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�DŻ�D���D�>�D�~�Dƾ�D���D�;�D�~�DǾ�D���D�>�Dȁ�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�(R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aا�AؤAؤAؠ�A؞�A؟�A؞�A؝A؞�Aؠ'AءbAؗ�A؏(A،�A؉�A؅A؃�A؄�A؅�A؅A؂uA�{�A�{�A�|�A�y�A�yrA�xlA�v�A�v`A�poA�CA��A��9A�[�A��9A�	AŕA�5�A��A��A��A�7�A�֡A�|PA���A�GEA�A��DA���A�k�A��A�A�=A�7�A��#A�PA��:A� \A�_A���A�{JA�̘A���A�y�A�gA�]�A���A��A��jA���A��A���A���A�YA�h�A�� A�ZQA�F�A��A�ҽA�'�A��A��A��:A��]A�l�A�~�A�$A{��Ay�At��AqAj(�Ad$A_ϫA\�AYV�AV��AT� AQ�AL�AJ�_AH�BAFH�AE��AC�6AB�SA@\�A>�OA<��A;TaA9�5A6�A3b�A1�9A.�5A,ɆA,+kA+��A,�A,0UA+6A(qA(�{A(@OA'+kA%��A&� A%�A$��A#�A"��A!Q�A�AA�A/�Ac AAc�A1�A�AخA��A|�Ah�AQ�A�IAɆA�hA�IAxAB[A�A�A��A��AN<A�qA7�A	lADgA��Az�A�]AMjA�aA*�AĜA��A��AtTA�A��A�AѷA��A�AaA
�ZA
�	A
	�A	��A	�A��A6zAJA��A�AAe�A9�A�A$�AVA�AY�AC-A��AMjA�0A\)AS�A�A<6A �6A �CA �A �@�IR@�Q�@��@�R�@��$@���@�خ@��@�O@�ƨ@�s�@��@�`�@��@�1'@�=@�ں@��j@�%@� �@��&@��@��f@�x@ﯸ@�YK@�v`@��|@��@�Q�@�͟@��@��@��@�7L@�.@�!@��@��P@�`�@�t@�!-@�Ɇ@�=q@�G@�ݘ@��,@�w�@�K^@�H@�j@�V�@�@��K@��@߼�@�@�/@ܦL@܇+@�6�@��@���@��@�C@��@��@ڝI@�P�@�YK@׽�@���@��U@�e@զ�@�Q�@�E9@�J#@���@�ȴ@�W�@�e,@��/@�M@�G@ѶF@�C�@�-�@�e,@�͟@�7�@�	@�b@� �@��d@�E9@�-w@̜x@�@ʭ�@�F�@Ȗ�@��@��2@�j@��D@���@�X�@Ŀ�@�y>@�_@�{@�4@ò-@�.I@���@�@�u�@�	@���@���@�RT@��@��@�҉@��r@�hs@���@�PH@��@�2a@��)@���@��>@���@���@�<�@��^@�s�@��@�V@���@�;@���@�8�@���@�Q�@�@���@�"h@���@�;@���@�1�@���@�`B@�/�@�V@��.@��o@��)@��
@�j�@��@��F@�RT@���@�z@�&�@��*@��j@�y>@�h
@��@�v`@��@�u�@�A�@� �@�_@���@��:@�@��@���@��s@���@���@�h�@�E�@���@��@�n/@�G�@��`@��@���@�YK@��o@���@�{J@�+�@���@��j@��\@���@�h�@�?@�)�@��@��@�iD@�"�@��@�ߤ@�͟@���@���@�m�@�A�@�b@�	@���@��t@��V@��@�o�@��@��c@���@��@�|�@�H�@��@��C@�Q�@�8@��@���@�ѷ@��@�d�@��;@�iD@�F�@��@���@���@�?�@�o @�33@���@���@�Z@� �@�@���@���@�
=@���@���@�	@�1'@���@��@���@�i�@�7@��h@�@O@�4@��	@�r�@��@��@��@���@��@��4@��@��@���@�GE@���@��@���@�f�@��@�l�@�Ta@�_@�L0@��@���@�p�@�o�@�t�@�rG@�=@��@�q@�Ft@��@��P@�C@���@���@�tT@�@�@���@�o�@�K�@�8�@� \@� i@��2@�Ĝ@��@��@�=q@�~@��]@��N@��@��@�x�@�o�@�@O@��@���@���@���@�:�@��3@���@�j@�%@��)@�bN@�1�@�@�x@���@��}@�j@���@��[@��z@��@�H@��@��@�n/@�E9@��8@��U@���@��u@�y>@�C-@��@j�@~�@|�/@|%�@{��@z��@z�@y�j@y�h@y\�@y<6@y�@x��@x�)@x��@xG@w��@wRT@w!-@v�R@v!�@uT�@t��@t��@t�@t��@tM@s��@s@r~�@r@q�X@q \@p�_@o��@oC@n��@n:*@m�@m�M@l�@lZ@k�A@k�@k~�@j�@j��@j!�@i��@i�@h~(@g��@gS@f��@f�F@fB[@f@e��@e�z@eo @eDg@e+�@d�z@c�]@c��@cRT@c1�@cS@bc @a��@arG@`�@`r�@`D�@`�@_�g@_P�@_�@_�@^��@^v�@^&�@]�H@]�@]s�@]!�@\�9@\�@[�a@[K�@Z�m@Z&�@Y��@YS&@X��@X�@W��@W i@V��@V0U@U�z@UL�@T�f@Tg8@S��@SK�@R�h@R��@Rs�@RZ�@Q�Z@Qc�@Q+@P�_@P��@P:�@O�@O,�@N��@NkQ@M��@M��@Ms�@M/@L��@L��@L�u@LN�@K��@K�@Kخ@K��@K�f@K8@J�A@J	@I��@I�=@I4@H��@H��@H��@H1'@G�Q@G�4@G�@F��@FO@E��@E�@DɆ@D[�@D,=@C� @C�f@B�M@B��@BO@A�@Ao @A8�@A+�@A@@A�@@�|@@��@@l"@@M@@�@?�
@?�P@>�"@>V@>�@=��@=��@=��@=�H@=f�@<�@<oi@<Z@<H@;�;@;s@:ں@:��@:�@:6�@9�@9�@9�^@9�n@9��@9|@9k�@9X@9(�@8Ɇ@8H@8 �@7�m@7|�@7O@7@6��@6ff@6$�@5�d@5j@5�@4�5@4�)@4q@4M@3�w@3qv@3@O@31�@2�M@2�@2�\@2M�@2)�@2u@1��@1�@1a�@1J�@15�@1#�@1q@1@0��@0��@0l"@0:�@07�@02�@0  @/�w@/�f@/�@.��@.��@.R�@.&�@-�@-��@-��@-^�@-�@,��@,/�@+�m@+�0@+��@+@*�x@*0U@)�@)��@)w2@) \@(�$@(I�@'�F@'e�@'F�@&��@&��@&	@%�T@%ԕ@%��@%c@%q@$�$@$��@$�@$bN@$<�@$�@#�a@#�k@#�	@#]�@#"�@"u%@!�@!zx@!�@ ��@ ��@ H@�r@��@��@�{@�c@��@�r@6�@@��@J�@	l@�@�9@y>@(�@�*@a@9�@�]@Z�@1�@
�@�@��@�@ѷ@�@��@r�@Q�@4n@�K@iD@J#@+@o@�8@�+@_@�#@�3@��@�~@?}@��@�@]d@,=@�]@�@�@�@�}@��@�q@��@6z@ں@B[@�@��@��@�z@�@��@�n@X@�@�@�@��@�@Ft@ �@�@� @ƨ@�k@]�@��@6�@($@�@�@�z@��@X@?}@0�@-w@@֡@�@�u@U2@ �@G@�@�K@��@X�@�@
�8@
�H@
��@
��@
p;@
\�@
H�@
_@	�@	@	�n@	��@	hs@	L�@	(�@��@�v@�@��@��@�@tT@j@Xy@M@I�@D�@2�@�@x@��@˒@�*@\)@9�@&@�@��@�c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aا�AؤAؤAؠ�A؞�A؟�A؞�A؝A؞�Aؠ'AءbAؗ�A؏(A،�A؉�A؅A؃�A؄�A؅�A؅A؂uA�{�A�{�A�|�A�y�A�yrA�xlA�v�A�v`A�poA�CA��A��9A�[�A��9A�	AŕA�5�A��A��A��A�7�A�֡A�|PA���A�GEA�A��DA���A�k�A��A�A�=A�7�A��#A�PA��:A� \A�_A���A�{JA�̘A���A�y�A�gA�]�A���A��A��jA���A��A���A���A�YA�h�A�� A�ZQA�F�A��A�ҽA�'�A��A��A��:A��]A�l�A�~�A�$A{��Ay�At��AqAj(�Ad$A_ϫA\�AYV�AV��AT� AQ�AL�AJ�_AH�BAFH�AE��AC�6AB�SA@\�A>�OA<��A;TaA9�5A6�A3b�A1�9A.�5A,ɆA,+kA+��A,�A,0UA+6A(qA(�{A(@OA'+kA%��A&� A%�A$��A#�A"��A!Q�A�AA�A/�Ac AAc�A1�A�AخA��A|�Ah�AQ�A�IAɆA�hA�IAxAB[A�A�A��A��AN<A�qA7�A	lADgA��Az�A�]AMjA�aA*�AĜA��A��AtTA�A��A�AѷA��A�AaA
�ZA
�	A
	�A	��A	�A��A6zAJA��A�AAe�A9�A�A$�AVA�AY�AC-A��AMjA�0A\)AS�A�A<6A �6A �CA �A �@�IR@�Q�@��@�R�@��$@���@�خ@��@�O@�ƨ@�s�@��@�`�@��@�1'@�=@�ں@��j@�%@� �@��&@��@��f@�x@ﯸ@�YK@�v`@��|@��@�Q�@�͟@��@��@��@�7L@�.@�!@��@��P@�`�@�t@�!-@�Ɇ@�=q@�G@�ݘ@��,@�w�@�K^@�H@�j@�V�@�@��K@��@߼�@�@�/@ܦL@܇+@�6�@��@���@��@�C@��@��@ڝI@�P�@�YK@׽�@���@��U@�e@զ�@�Q�@�E9@�J#@���@�ȴ@�W�@�e,@��/@�M@�G@ѶF@�C�@�-�@�e,@�͟@�7�@�	@�b@� �@��d@�E9@�-w@̜x@�@ʭ�@�F�@Ȗ�@��@��2@�j@��D@���@�X�@Ŀ�@�y>@�_@�{@�4@ò-@�.I@���@�@�u�@�	@���@���@�RT@��@��@�҉@��r@�hs@���@�PH@��@�2a@��)@���@��>@���@���@�<�@��^@�s�@��@�V@���@�;@���@�8�@���@�Q�@�@���@�"h@���@�;@���@�1�@���@�`B@�/�@�V@��.@��o@��)@��
@�j�@��@��F@�RT@���@�z@�&�@��*@��j@�y>@�h
@��@�v`@��@�u�@�A�@� �@�_@���@��:@�@��@���@��s@���@���@�h�@�E�@���@��@�n/@�G�@��`@��@���@�YK@��o@���@�{J@�+�@���@��j@��\@���@�h�@�?@�)�@��@��@�iD@�"�@��@�ߤ@�͟@���@���@�m�@�A�@�b@�	@���@��t@��V@��@�o�@��@��c@���@��@�|�@�H�@��@��C@�Q�@�8@��@���@�ѷ@��@�d�@��;@�iD@�F�@��@���@���@�?�@�o @�33@���@���@�Z@� �@�@���@���@�
=@���@���@�	@�1'@���@��@���@�i�@�7@��h@�@O@�4@��	@�r�@��@��@��@���@��@��4@��@��@���@�GE@���@��@���@�f�@��@�l�@�Ta@�_@�L0@��@���@�p�@�o�@�t�@�rG@�=@��@�q@�Ft@��@��P@�C@���@���@�tT@�@�@���@�o�@�K�@�8�@� \@� i@��2@�Ĝ@��@��@�=q@�~@��]@��N@��@��@�x�@�o�@�@O@��@���@���@���@�:�@��3@���@�j@�%@��)@�bN@�1�@�@�x@���@��}@�j@���@��[@��z@��@�H@��@��@�n/@�E9@��8@��U@���@��u@�y>@�C-@��@j�@~�@|�/@|%�@{��@z��@z�@y�j@y�h@y\�@y<6@y�@x��@x�)@x��@xG@w��@wRT@w!-@v�R@v!�@uT�@t��@t��@t�@t��@tM@s��@s@r~�@r@q�X@q \@p�_@o��@oC@n��@n:*@m�@m�M@l�@lZ@k�A@k�@k~�@j�@j��@j!�@i��@i�@h~(@g��@gS@f��@f�F@fB[@f@e��@e�z@eo @eDg@e+�@d�z@c�]@c��@cRT@c1�@cS@bc @a��@arG@`�@`r�@`D�@`�@_�g@_P�@_�@_�@^��@^v�@^&�@]�H@]�@]s�@]!�@\�9@\�@[�a@[K�@Z�m@Z&�@Y��@YS&@X��@X�@W��@W i@V��@V0U@U�z@UL�@T�f@Tg8@S��@SK�@R�h@R��@Rs�@RZ�@Q�Z@Qc�@Q+@P�_@P��@P:�@O�@O,�@N��@NkQ@M��@M��@Ms�@M/@L��@L��@L�u@LN�@K��@K�@Kخ@K��@K�f@K8@J�A@J	@I��@I�=@I4@H��@H��@H��@H1'@G�Q@G�4@G�@F��@FO@E��@E�@DɆ@D[�@D,=@C� @C�f@B�M@B��@BO@A�@Ao @A8�@A+�@A@@A�@@�|@@��@@l"@@M@@�@?�
@?�P@>�"@>V@>�@=��@=��@=��@=�H@=f�@<�@<oi@<Z@<H@;�;@;s@:ں@:��@:�@:6�@9�@9�@9�^@9�n@9��@9|@9k�@9X@9(�@8Ɇ@8H@8 �@7�m@7|�@7O@7@6��@6ff@6$�@5�d@5j@5�@4�5@4�)@4q@4M@3�w@3qv@3@O@31�@2�M@2�@2�\@2M�@2)�@2u@1��@1�@1a�@1J�@15�@1#�@1q@1@0��@0��@0l"@0:�@07�@02�@0  @/�w@/�f@/�@.��@.��@.R�@.&�@-�@-��@-��@-^�@-�@,��@,/�@+�m@+�0@+��@+@*�x@*0U@)�@)��@)w2@) \@(�$@(I�@'�F@'e�@'F�@&��@&��@&	@%�T@%ԕ@%��@%c@%q@$�$@$��@$�@$bN@$<�@$�@#�a@#�k@#�	@#]�@#"�@"u%@!�@!zx@!�@ ��@ ��@ H@�r@��@��@�{@�c@��@�r@6�@@��@J�@	l@�@�9@y>@(�@�*@a@9�@�]@Z�@1�@
�@�@��@�@ѷ@�@��@r�@Q�@4n@�K@iD@J#@+@o@�8@�+@_@�#@�3@��@�~@?}@��@�@]d@,=@�]@�@�@�@�}@��@�q@��@6z@ں@B[@�@��@��@�z@�@��@�n@X@�@�@�@��@�@Ft@ �@�@� @ƨ@�k@]�@��@6�@($@�@�@�z@��@X@?}@0�@-w@@֡@�@�u@U2@ �@G@�@�K@��@X�@�@
�8@
�H@
��@
��@
p;@
\�@
H�@
_@	�@	@	�n@	��@	hs@	L�@	(�@��@�v@�@��@��@�@tT@j@Xy@M@I�@D�@2�@�@x@��@˒@�*@\)@9�@&@�@��@�c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
NB
NB
NB
NB
NB
hB
�B
�B
�B
�B
4B
 B
 B
 B
�B
�B
�B
�B
�B
�B
HB
}B
}B
HB
.B
�B
�B
�B
�B
	7B	�iB	�#B	ںB	��B	�B
�B
�B
�B
�B
 vB
%zB
)�B
lqB
l"B
o B
vzB
y�B
|�B
��B
��B
�RB
��B�BxB!Bd&B�B��B��B��B��B�*B��B�B��BkQBEmBD�BshB��B��B��Bm�BeFBe`B3�B'�BVB�B
��B
�}B
a�B
I7B
A;B
)*B
B	�)B	��B	��B	��B	xB	X+B	?�B	-wB	�B	�B	�B�]B�nB�VB��B�"B��B̈́B՛B҉B��B�]B��B�wB�eB�B�"B�"B��BāB�SB��B��B	B	B	�B	pB	0�B	7�B	-�B	H�B	H�B	E9B	CB	L�B	\]B	e,B	`BB	W�B	W
B	\xB	c B	\]B	aHB	e�B	jB	l�B	n}B	q'B	��B	�B	��B	��B	��B	�5B	�%B	�XB	�	B	��B	�6B	�B	�4B	�B	��B	��B	�HB	�B	�B	��B	��B	�B	�B	�EB	��B	�B	ŢB	��B	�7B	�^B	�VB	ΥB	�TB	��B	յB	��B	�@B	�B	�9B	�
B	�_B	ٚB	�kB	�kB	ڠB	�1B	�_B	�?B	��B	��B	ʦB	�=B	�lB	��B	��B	�B	͟B	��B	��B	��B	�}B	οB	��B	��B	՛B	�9B	ּB	רB	��B	��B	ٚB	��B	�kB	�B	��B	�kB	�B	�7B	ںB	�	B	�qB	��B	�=B	ۦB	�#B	�qB	�WB	�qB	�!B	�B	�IB	��B	�B	��B	�B	�~B	ݘB	ݘB	�B	ߤB	�B	�B	��B	�VB	ߤB	�-B	�4B	��B	�B	��B	�B	��B	�B	��B	�B	�B	�B	�B	�_B	�*B	�*B	�0B	�B	��B	�;B	��B	�|B	�B	�aB	�;B	�B	�B	�B	�;B	�B	�B	�B	�'B	�vB	��B	�B	�3B	�'B	�B	��B	��B	��B	�B	�B	��B	�IB	�}B	�}B	�}B	�B	�}B	�IB	��B	�iB	��B	��B	�fB	�,B	�B	�B	�:B	�B	��B	�8B	�fB	�B	��B	�>B	�B	�"B	�WB	��B	�CB	�wB	��B	�]B	��B	��B	��B	��B	�IB	�IB	�oB	�B	�B	��B	�B	��B	��B	�B	��B	�|B	�B	��B	��B	�B	��B	�B	��B	�ZB	��B	��B	��B	�FB	��B	�LB	��B	��B	��B	��B	��B	�lB	�RB	�B	�	B	��B	��B	�*B	�B	��B	�"B	��B	��B	�BB	��B	�BB	��B	��B	�HB	��B	��B
 4B
 �B
 �B
 �B
 B
B
�B
�B
�B
�B
�B
�B
B
B
'B
uB
�B
B
�B
3B
�B
B
9B
B
�B
�B
+B
zB
�B
fB
KB
KB
�B
	�B

XB

	B
	�B
	�B

	B

	B

XB
B
�B
6B
�B
jB
PB
6B
�B
B
�B
�B
�B
B
B
"B
VB
�B
VB
<B
�B
�B
�B
HB
NB
NB
hB
�B
TB
�B
�B
�B
�B
B
�B
[B
�B
�B
�B
aB
B
aB
MB
�B
9B
�B
_B
�B
+B
�B
�B
�B
�B
eB
B
WB
=B
=B
	B
)B
xB
�B
�B
pB
 'B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
!B
 vB
 \B
!B
!�B
"NB
!�B
!�B
"hB
#:B
# B
#:B
#:B
#TB
# B
#B
"�B
#:B
$&B
$@B
$tB
$B
$B
#�B
$&B
$&B
$&B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%FB
&2B
&�B
&�B
&�B
'mB
'�B
(
B
(XB
)�B
)�B
)�B
)�B
)�B
*B
+kB
+�B
,�B
-wB
-�B
-]B
-�B
.�B
/5B
/OB
/iB
.�B
.�B
/iB
/iB
/�B
0oB
0�B
1B
1[B
1�B
2|B
2�B
3hB
3�B
4�B
5%B
5�B
6FB
6�B
6�B
6�B
6�B
7B
72B
72B
72B
7fB
7�B
8B
8B
8B
88B
8�B
9>B
9rB
9rB
9�B
9rB
9�B
9�B
:^B
:^B
:DB
:�B
:�B
;0B
;�B
<B
<PB
<�B
<�B
<�B
<�B
=VB
=qB
=VB
=�B
=�B
=�B
>BB
>�B
?.B
?cB
@B
@OB
@�B
@�B
@�B
@�B
@�B
AB
AUB
AUB
A;B
A�B
BAB
B[B
B�B
B�B
B�B
CB
C-B
C{B
C�B
DB
D3B
D3B
DMB
D�B
D�B
D�B
EB
E9B
ESB
E�B
E�B
E�B
E�B
F%B
F�B
F�B
F�B
GEB
GzB
G�B
G�B
H1B
H�B
H�B
I7B
IlB
I�B
J#B
J�B
J�B
J�B
KB
K)B
K�B
KxB
K�B
K�B
L0B
K�B
LdB
L�B
L~B
L�B
MjB
M�B
M�B
NB
NpB
NpB
N�B
N�B
N�B
N�B
OB
O(B
OBB
O\B
O\B
OvB
O�B
O�B
PHB
P�B
P�B
Q B
QhB
Q�B
QhB
QhB
Q�B
Q�B
RB
R:B
R�B
R�B
R�B
SuB
S�B
TB
TB
TFB
T{B
T�B
UB
UgB
U�B
VB
VB
VB
V9B
VB
V9B
V�B
V�B
V�B
V�B
W
B
W$B
W�B
XEB
X_B
XyB
X_B
XyB
X_B
X�B
Y1B
YeB
YKB
YKB
Y�B
Y�B
ZkB
Z7B
ZQB
Z�B
[#B
[	B
[#B
[	B
[=B
[=B
[#B
[#B
[#B
[qB
[�B
[�B
[�B
\)B
\)B
\xB
\�B
]B
]dB
]�B
^B
^5B
^5B
^5B
^OB
^�B
^�B
_B
_!B
_!B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
aB
a�B
a|B
a|B
a|B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
cB
cnB
cTB
cnB
c�B
c�B
dZB
d�B
d�B
d�B
d�B
ezB
e�B
fB
e�B
ffB
f�B
f�B
g8B
g�B
g�B
g�B
h�B
hXB
h�B
h�B
iB
iB
i*B
i�B
i�B
i�B
i�B
jB
j0B
jKB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
mB
m)B
m]B
mCB
m�B
nB
n/B
n}B
n�B
oB
oiB
o�B
o�B
o�B
p!B
poB
p�B
q'B
q'B
q�B
q�B
r-B
r-B
raB
r|B
sB
shB
sMB
shB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
vB
v+B
v+B
vzB
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xlB
x�B
y�B
y�B
y�B
y�B
zB
y�B
zB
y�B
zDB
z�B
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
{�B
{�B
{�B
|�B
}B
}B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
B
HB
�B
�B
� B
�B
�B
�iB
��B
�;B
��B
��B
�'B
�'B
�'B
�[B
�AB
�'B
�AB
�[B
��B
��B
�B
�GB
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�MB
�MB
��B
��B
��B
�B
�B
�B
�SB
�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
NB
NB
NB
NB
NB
hB
�B
�B
�B
�B
4B
 B
 B
 B
�B
�B
�B
�B
�B
�B
HB
}B
}B
HB
.B
�B
�B
�B
�B
	7B	�iB	�#B	ںB	��B	�B
�B
�B
�B
�B
 vB
%zB
)�B
lqB
l"B
o B
vzB
y�B
|�B
��B
��B
�RB
��B�BxB!Bd&B�B��B��B��B��B�*B��B�B��BkQBEmBD�BshB��B��B��Bm�BeFBe`B3�B'�BVB�B
��B
�}B
a�B
I7B
A;B
)*B
B	�)B	��B	��B	��B	xB	X+B	?�B	-wB	�B	�B	�B�]B�nB�VB��B�"B��B̈́B՛B҉B��B�]B��B�wB�eB�B�"B�"B��BāB�SB��B��B	B	B	�B	pB	0�B	7�B	-�B	H�B	H�B	E9B	CB	L�B	\]B	e,B	`BB	W�B	W
B	\xB	c B	\]B	aHB	e�B	jB	l�B	n}B	q'B	��B	�B	��B	��B	��B	�5B	�%B	�XB	�	B	��B	�6B	�B	�4B	�B	��B	��B	�HB	�B	�B	��B	��B	�B	�B	�EB	��B	�B	ŢB	��B	�7B	�^B	�VB	ΥB	�TB	��B	յB	��B	�@B	�B	�9B	�
B	�_B	ٚB	�kB	�kB	ڠB	�1B	�_B	�?B	��B	��B	ʦB	�=B	�lB	��B	��B	�B	͟B	��B	��B	��B	�}B	οB	��B	��B	՛B	�9B	ּB	רB	��B	��B	ٚB	��B	�kB	�B	��B	�kB	�B	�7B	ںB	�	B	�qB	��B	�=B	ۦB	�#B	�qB	�WB	�qB	�!B	�B	�IB	��B	�B	��B	�B	�~B	ݘB	ݘB	�B	ߤB	�B	�B	��B	�VB	ߤB	�-B	�4B	��B	�B	��B	�B	��B	�B	��B	�B	�B	�B	�B	�_B	�*B	�*B	�0B	�B	��B	�;B	��B	�|B	�B	�aB	�;B	�B	�B	�B	�;B	�B	�B	�B	�'B	�vB	��B	�B	�3B	�'B	�B	��B	��B	��B	�B	�B	��B	�IB	�}B	�}B	�}B	�B	�}B	�IB	��B	�iB	��B	��B	�fB	�,B	�B	�B	�:B	�B	��B	�8B	�fB	�B	��B	�>B	�B	�"B	�WB	��B	�CB	�wB	��B	�]B	��B	��B	��B	��B	�IB	�IB	�oB	�B	�B	��B	�B	��B	��B	�B	��B	�|B	�B	��B	��B	�B	��B	�B	��B	�ZB	��B	��B	��B	�FB	��B	�LB	��B	��B	��B	��B	��B	�lB	�RB	�B	�	B	��B	��B	�*B	�B	��B	�"B	��B	��B	�BB	��B	�BB	��B	��B	�HB	��B	��B
 4B
 �B
 �B
 �B
 B
B
�B
�B
�B
�B
�B
�B
B
B
'B
uB
�B
B
�B
3B
�B
B
9B
B
�B
�B
+B
zB
�B
fB
KB
KB
�B
	�B

XB

	B
	�B
	�B

	B

	B

XB
B
�B
6B
�B
jB
PB
6B
�B
B
�B
�B
�B
B
B
"B
VB
�B
VB
<B
�B
�B
�B
HB
NB
NB
hB
�B
TB
�B
�B
�B
�B
B
�B
[B
�B
�B
�B
aB
B
aB
MB
�B
9B
�B
_B
�B
+B
�B
�B
�B
�B
eB
B
WB
=B
=B
	B
)B
xB
�B
�B
pB
 'B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
!B
 vB
 \B
!B
!�B
"NB
!�B
!�B
"hB
#:B
# B
#:B
#:B
#TB
# B
#B
"�B
#:B
$&B
$@B
$tB
$B
$B
#�B
$&B
$&B
$&B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%FB
&2B
&�B
&�B
&�B
'mB
'�B
(
B
(XB
)�B
)�B
)�B
)�B
)�B
*B
+kB
+�B
,�B
-wB
-�B
-]B
-�B
.�B
/5B
/OB
/iB
.�B
.�B
/iB
/iB
/�B
0oB
0�B
1B
1[B
1�B
2|B
2�B
3hB
3�B
4�B
5%B
5�B
6FB
6�B
6�B
6�B
6�B
7B
72B
72B
72B
7fB
7�B
8B
8B
8B
88B
8�B
9>B
9rB
9rB
9�B
9rB
9�B
9�B
:^B
:^B
:DB
:�B
:�B
;0B
;�B
<B
<PB
<�B
<�B
<�B
<�B
=VB
=qB
=VB
=�B
=�B
=�B
>BB
>�B
?.B
?cB
@B
@OB
@�B
@�B
@�B
@�B
@�B
AB
AUB
AUB
A;B
A�B
BAB
B[B
B�B
B�B
B�B
CB
C-B
C{B
C�B
DB
D3B
D3B
DMB
D�B
D�B
D�B
EB
E9B
ESB
E�B
E�B
E�B
E�B
F%B
F�B
F�B
F�B
GEB
GzB
G�B
G�B
H1B
H�B
H�B
I7B
IlB
I�B
J#B
J�B
J�B
J�B
KB
K)B
K�B
KxB
K�B
K�B
L0B
K�B
LdB
L�B
L~B
L�B
MjB
M�B
M�B
NB
NpB
NpB
N�B
N�B
N�B
N�B
OB
O(B
OBB
O\B
O\B
OvB
O�B
O�B
PHB
P�B
P�B
Q B
QhB
Q�B
QhB
QhB
Q�B
Q�B
RB
R:B
R�B
R�B
R�B
SuB
S�B
TB
TB
TFB
T{B
T�B
UB
UgB
U�B
VB
VB
VB
V9B
VB
V9B
V�B
V�B
V�B
V�B
W
B
W$B
W�B
XEB
X_B
XyB
X_B
XyB
X_B
X�B
Y1B
YeB
YKB
YKB
Y�B
Y�B
ZkB
Z7B
ZQB
Z�B
[#B
[	B
[#B
[	B
[=B
[=B
[#B
[#B
[#B
[qB
[�B
[�B
[�B
\)B
\)B
\xB
\�B
]B
]dB
]�B
^B
^5B
^5B
^5B
^OB
^�B
^�B
_B
_!B
_!B
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
aB
a�B
a|B
a|B
a|B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
cB
cnB
cTB
cnB
c�B
c�B
dZB
d�B
d�B
d�B
d�B
ezB
e�B
fB
e�B
ffB
f�B
f�B
g8B
g�B
g�B
g�B
h�B
hXB
h�B
h�B
iB
iB
i*B
i�B
i�B
i�B
i�B
jB
j0B
jKB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
mB
m)B
m]B
mCB
m�B
nB
n/B
n}B
n�B
oB
oiB
o�B
o�B
o�B
p!B
poB
p�B
q'B
q'B
q�B
q�B
r-B
r-B
raB
r|B
sB
shB
sMB
shB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
vB
v+B
v+B
vzB
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xlB
x�B
y�B
y�B
y�B
y�B
zB
y�B
zB
y�B
zDB
z�B
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
{�B
{�B
{�B
|�B
}B
}B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~wB
~�B
~�B
~�B
~�B
B
HB
�B
�B
� B
�B
�B
�iB
��B
�;B
��B
��B
�'B
�'B
�'B
�[B
�AB
�'B
�AB
�[B
��B
��B
�B
�GB
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�MB
�MB
��B
��B
��B
�B
�B
�B
�SB
�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192056  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192056  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192056                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042105  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042105  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                