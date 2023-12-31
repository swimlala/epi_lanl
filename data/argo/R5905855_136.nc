CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-04T06:50:47Z creation;2022-11-04T06:50:48Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221104065047  20221104070739  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��E+�e1   @��Eg(��@-�O�;dZ�c��`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A���A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBz  B���B�  B���B�  B�33B�33B�  B�  B���B���B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C  C�fC�fC  C
  C  C  C  C  C�C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\L�C]�3C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@}p�@��R@��RA\)A?\)A_\)A�z�A��A��A��A��GAϮA߮A�A��B�
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
Bp=pBy�
B��RB��B��RB��B��B��B��B��B��RB��RB��RB��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C (�C��C�)C�)C��C	��C��C��C��C��C]C��C��C��C��C��C��C"]C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C\B�C]��C_�)Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C�{C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDB�DB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�xR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�LdA�L�A�M�A�MA�L�A�NpA�OvA�OvA�PA�R�A�PHA�S[A�VA�PA�HA�H�A�FA�I�A�J#A�FA�C�A�C�A�@�A�A A�@�A�;0A�&�A��rA�_A֏�A�eA���AԐ�A���A�AĊrA�D�A��TA�/�A��]A���A�EmA��OA��A�B�A�OA�]�A���A�$tA��tA��A��A��RA�NpA��A���A�`BA�T�A�rA� �A�� A��A��>A�~A��>A�33A��A�Z�A�S&A��A��{A���A���A� �A�#�A�I�A��;A��.A���A�WsA��A��/A|ںAw��Av4As�AqMAo�Al�Ak֡Ai|�Ag�[Af�Ab�.Aa.�A^A[�"AZ��AY��AX�&AUPHAR�]AQ��AN�DAK~AIu%AG��AE��AC�"ACt�A@�kA<��A9�gA7��A5�+A24nA0u�A/w2A-�A,K�A(�8A'��A'z�A&�A%l"A%^�A%N�A$�A$��A#��A#X�A"�bA"o�A"c�A"	A"B[A"HA!�A ?�ARTA��A��AYKA �A�Ae�A�OA*�A��A�A0�AjA�SAAS&AW?A��A�6A�ADgA�]A�/A�A=AA
��A
~�AA,=Aa�A��A�OAĜA
�	A	��A
/�A
K^A
�A	ɆAYKA>�A�+A�1Aj�Am]A��A~�A-�A_pA�]A�'A�HA[�A ��@�V@��I@�s@�-w@��@��@��@��4@��t@���@��{@��,@��N@�=�@��@��@���@��@���@��@��@���@�"�@�o@�C�@��N@�}�@�;d@�@��Y@�_p@�r@��@�^�@�J�@���@�#�@�l"@��>@�e,@�C�@�;@��@�z�@���@�"@�iD@慈@�G�@�!-@�;@�:�@��y@��@�U2@�=�@��@�M@ߚk@��B@ݘ�@��@��@�Y@�4�@�F�@���@��K@ܟ�@�N<@�Z�@�a@��@ڝI@�@�2a@؃�@��@�5�@���@֫6@�K^@��;@���@Շ�@�X�@��@ԯO@�-�@ӧ�@�L0@��'@Բ�@�}V@�1�@��@Ӟ�@�G�@�q@��@�Ĝ@с@��@�V�@���@�j�@��/@ίO@ε@έ�@�)�@͆�@̬�@��3@˿H@���@�[W@�y>@�6@�&�@���@ɑh@�v`@�n/@�_p@�/@ȟ�@��@�ԕ@�,�@�^5@Ů�@�!�@ı�@�_@þw@�+@�W?@�9�@�7@�s@�@��u@�g8@��@��$@��)@�j@��@��$@�P�@��@���@���@�;�@��@�o�@��"@��U@���@���@�A�@�/�@��@���@���@���@�p;@���@�%F@��@��E@�@�k�@��K@��o@�`�@��@���@�\)@���@�Xy@��#@�A�@���@��}@�h
@��@��@��D@�Q�@��+@��=@�<6@��|@���@���@�b�@�5�@��M@�~�@�H�@���@�E9@���@���@�h�@�
�@���@���@���@�f�@�5�@�o@��,@�ff@�!�@���@�zx@�4@� \@�o@��@���@�H�@� �@��@�@�qv@��@��@���@��$@���@���@���@�g8@�0U@�{@��j@�p�@�5�@��@���@�q�@�خ@��@�U�@��@�(@��8@���@�7@�7L@�ں@���@�9X@��;@��*@��7@�zx@���@���@�m]@��2@�T�@�zx@�K�@��@��
@��n@���@�zx@�Dg@�@@�;@���@��@���@�`B@�<6@���@�xl@��@��F@�u�@�F@��@���@��@�e@��9@��@���@�a�@�'�@�͟@��@�b@��r@��@���@��@���@�Vm@��@��`@��z@�e�@�b@���@�\�@�V@�֡@��U@��.@�r�@�M@�O@��g@���@��@�q@��?@���@�oi@�4n@��T@���@�N<@�V@�҉@���@�:�@�(�@�{@���@���@���@�c@�b�@�+�@��@��I@�H�@��@���@�zx@�=@�$t@��@��1@�R�@��+@���@�c@�dZ@��@��j@�xl@���@�s�@�+�@���@��D@�\�@�U2@�C-@�e@��@���@�\�@�Mj@�'�@��c@���@��@�B[@��@�@~�@~$�@}O�@|֡@{��@z8�@y�@xĜ@x~(@xA�@xM@w�K@w�@@wK�@v�b@v�\@v�F@vTa@u�@um]@t�@t �@s,�@rn�@qԕ@q��@qVm@p�@o˒@n��@n�@l��@l�Y@lPH@l"h@k��@kiD@k33@j�@jGE@i�@i��@i�7@iS&@h�/@h��@h�Y@h2�@h@g��@g�@gMj@f�!@fe@e��@e�t@eG�@d�@d`�@c��@c�k@cx@cW?@c!-@b�@b�@b��@b�!@bq�@b�@a�@aN<@a:�@`�E@`�u@`9X@`b@_��@_��@_K�@^��@^�<@^a|@^5?@]�D@]<6@\Ɇ@\�$@\��@\c�@[�@[K�@Z��@Y��@Y�9@Y�@Y�@Y��@X��@X*�@W�@@WE9@V��@Uԕ@T�v@Th�@T7@S��@S1�@R� @R=q@R{@Q?}@P�j@P?�@O��@Ox@N��@N�B@N�}@Nq�@NJ�@M�-@MDg@L�E@L�.@Loi@LH@K��@K�@J͟@JGE@I�@Ip�@H�v@Hw�@H:�@G�A@GO@F��@F.�@E�@E�@E��@D�|@D��@DM@Dx@C�a@C�f@CJ#@CC@B�@BQ@A��@AB�@A�@@�@@1'@?�@?�&@?�@?��@?�*@?�:@?dZ@?�@>��@>�x@>�F@>$�@>_@=�)@=�H@=Vm@<��@<�@<>B@<9X@<�@;��@;�@;n/@;@:��@:B[@:4@9��@9��@9��@9X@9#�@8�	@8�v@8��@8��@8��@8u�@8c�@8@7�;@7�@7;d@7!-@6�"@6�@6��@6Q@6�@5��@5c�@58�@4�j@4C-@3��@3�@3a@3�@2��@2�@1��@1�@1�@0��@0m�@/o�@.�@.�F@.~�@.p;@.4@-��@-��@-B�@-&�@,��@,tT@,M@+��@+� @+��@+o@*�m@*�@*�@*	@)m]@)*0@)@(�@(�@(�I@(m�@(G@'��@'�Q@'��@'�@'{J@'8@'�@'�@'�@&��@&�r@&8�@%�#@%�@%e,@%�@$�O@$PH@#��@#�Q@#��@"�@"� @"�+@"GE@"�@!��@!�@!�@!��@!hs@!c�@!a�@!e,@!f�@!(�@ �j@ q@ 4n@�+@ݘ@��@��@S�@�@�@��@�C@�@\�@�P@�9@w�@�@�m@خ@�a@�F@��@�@l�@(@�@�@�R@�L@�x@��@��@\�@&�@�@�@ϫ@��@^�@O�@B�@/@�@�`@��@�4@��@m�@*�@�@�&@��@�:@s@J#@�@��@z@Q@=q@�@�@�@zx@Vm@IR@+�@�@��@�@_@�@��@t�@S�@&@��@ں@��@�L@u%@\�@5?@-@O@�@ԕ@p�@T�@:�@#�@	l@�@Ɇ@�@j@D�@��@�@l�@]�@>�@��@�<@�F@��@}V@p;@J�@4@ϫ@��@O�@+�@�@�	@��@�U@��@h�@/�@1@ݘ@��@dZ@1�@$t@"�@"�@S@
ȴ@
��@
��@
z@
p;@
+k@
�@

�@	�=@	p�@	c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�LdA�L�A�M�A�MA�L�A�NpA�OvA�OvA�PA�R�A�PHA�S[A�VA�PA�HA�H�A�FA�I�A�J#A�FA�C�A�C�A�@�A�A A�@�A�;0A�&�A��rA�_A֏�A�eA���AԐ�A���A�AĊrA�D�A��TA�/�A��]A���A�EmA��OA��A�B�A�OA�]�A���A�$tA��tA��A��A��RA�NpA��A���A�`BA�T�A�rA� �A�� A��A��>A�~A��>A�33A��A�Z�A�S&A��A��{A���A���A� �A�#�A�I�A��;A��.A���A�WsA��A��/A|ںAw��Av4As�AqMAo�Al�Ak֡Ai|�Ag�[Af�Ab�.Aa.�A^A[�"AZ��AY��AX�&AUPHAR�]AQ��AN�DAK~AIu%AG��AE��AC�"ACt�A@�kA<��A9�gA7��A5�+A24nA0u�A/w2A-�A,K�A(�8A'��A'z�A&�A%l"A%^�A%N�A$�A$��A#��A#X�A"�bA"o�A"c�A"	A"B[A"HA!�A ?�ARTA��A��AYKA �A�Ae�A�OA*�A��A�A0�AjA�SAAS&AW?A��A�6A�ADgA�]A�/A�A=AA
��A
~�AA,=Aa�A��A�OAĜA
�	A	��A
/�A
K^A
�A	ɆAYKA>�A�+A�1Aj�Am]A��A~�A-�A_pA�]A�'A�HA[�A ��@�V@��I@�s@�-w@��@��@��@��4@��t@���@��{@��,@��N@�=�@��@��@���@��@���@��@��@���@�"�@�o@�C�@��N@�}�@�;d@�@��Y@�_p@�r@��@�^�@�J�@���@�#�@�l"@��>@�e,@�C�@�;@��@�z�@���@�"@�iD@慈@�G�@�!-@�;@�:�@��y@��@�U2@�=�@��@�M@ߚk@��B@ݘ�@��@��@�Y@�4�@�F�@���@��K@ܟ�@�N<@�Z�@�a@��@ڝI@�@�2a@؃�@��@�5�@���@֫6@�K^@��;@���@Շ�@�X�@��@ԯO@�-�@ӧ�@�L0@��'@Բ�@�}V@�1�@��@Ӟ�@�G�@�q@��@�Ĝ@с@��@�V�@���@�j�@��/@ίO@ε@έ�@�)�@͆�@̬�@��3@˿H@���@�[W@�y>@�6@�&�@���@ɑh@�v`@�n/@�_p@�/@ȟ�@��@�ԕ@�,�@�^5@Ů�@�!�@ı�@�_@þw@�+@�W?@�9�@�7@�s@�@��u@�g8@��@��$@��)@�j@��@��$@�P�@��@���@���@�;�@��@�o�@��"@��U@���@���@�A�@�/�@��@���@���@���@�p;@���@�%F@��@��E@�@�k�@��K@��o@�`�@��@���@�\)@���@�Xy@��#@�A�@���@��}@�h
@��@��@��D@�Q�@��+@��=@�<6@��|@���@���@�b�@�5�@��M@�~�@�H�@���@�E9@���@���@�h�@�
�@���@���@���@�f�@�5�@�o@��,@�ff@�!�@���@�zx@�4@� \@�o@��@���@�H�@� �@��@�@�qv@��@��@���@��$@���@���@���@�g8@�0U@�{@��j@�p�@�5�@��@���@�q�@�خ@��@�U�@��@�(@��8@���@�7@�7L@�ں@���@�9X@��;@��*@��7@�zx@���@���@�m]@��2@�T�@�zx@�K�@��@��
@��n@���@�zx@�Dg@�@@�;@���@��@���@�`B@�<6@���@�xl@��@��F@�u�@�F@��@���@��@�e@��9@��@���@�a�@�'�@�͟@��@�b@��r@��@���@��@���@�Vm@��@��`@��z@�e�@�b@���@�\�@�V@�֡@��U@��.@�r�@�M@�O@��g@���@��@�q@��?@���@�oi@�4n@��T@���@�N<@�V@�҉@���@�:�@�(�@�{@���@���@���@�c@�b�@�+�@��@��I@�H�@��@���@�zx@�=@�$t@��@��1@�R�@��+@���@�c@�dZ@��@��j@�xl@���@�s�@�+�@���@��D@�\�@�U2@�C-@�e@��@���@�\�@�Mj@�'�@��c@���@��@�B[@��@�@~�@~$�@}O�@|֡@{��@z8�@y�@xĜ@x~(@xA�@xM@w�K@w�@@wK�@v�b@v�\@v�F@vTa@u�@um]@t�@t �@s,�@rn�@qԕ@q��@qVm@p�@o˒@n��@n�@l��@l�Y@lPH@l"h@k��@kiD@k33@j�@jGE@i�@i��@i�7@iS&@h�/@h��@h�Y@h2�@h@g��@g�@gMj@f�!@fe@e��@e�t@eG�@d�@d`�@c��@c�k@cx@cW?@c!-@b�@b�@b��@b�!@bq�@b�@a�@aN<@a:�@`�E@`�u@`9X@`b@_��@_��@_K�@^��@^�<@^a|@^5?@]�D@]<6@\Ɇ@\�$@\��@\c�@[�@[K�@Z��@Y��@Y�9@Y�@Y�@Y��@X��@X*�@W�@@WE9@V��@Uԕ@T�v@Th�@T7@S��@S1�@R� @R=q@R{@Q?}@P�j@P?�@O��@Ox@N��@N�B@N�}@Nq�@NJ�@M�-@MDg@L�E@L�.@Loi@LH@K��@K�@J͟@JGE@I�@Ip�@H�v@Hw�@H:�@G�A@GO@F��@F.�@E�@E�@E��@D�|@D��@DM@Dx@C�a@C�f@CJ#@CC@B�@BQ@A��@AB�@A�@@�@@1'@?�@?�&@?�@?��@?�*@?�:@?dZ@?�@>��@>�x@>�F@>$�@>_@=�)@=�H@=Vm@<��@<�@<>B@<9X@<�@;��@;�@;n/@;@:��@:B[@:4@9��@9��@9��@9X@9#�@8�	@8�v@8��@8��@8��@8u�@8c�@8@7�;@7�@7;d@7!-@6�"@6�@6��@6Q@6�@5��@5c�@58�@4�j@4C-@3��@3�@3a@3�@2��@2�@1��@1�@1�@0��@0m�@/o�@.�@.�F@.~�@.p;@.4@-��@-��@-B�@-&�@,��@,tT@,M@+��@+� @+��@+o@*�m@*�@*�@*	@)m]@)*0@)@(�@(�@(�I@(m�@(G@'��@'�Q@'��@'�@'{J@'8@'�@'�@'�@&��@&�r@&8�@%�#@%�@%e,@%�@$�O@$PH@#��@#�Q@#��@"�@"� @"�+@"GE@"�@!��@!�@!�@!��@!hs@!c�@!a�@!e,@!f�@!(�@ �j@ q@ 4n@�+@ݘ@��@��@S�@�@�@��@�C@�@\�@�P@�9@w�@�@�m@خ@�a@�F@��@�@l�@(@�@�@�R@�L@�x@��@��@\�@&�@�@�@ϫ@��@^�@O�@B�@/@�@�`@��@�4@��@m�@*�@�@�&@��@�:@s@J#@�@��@z@Q@=q@�@�@�@zx@Vm@IR@+�@�@��@�@_@�@��@t�@S�@&@��@ں@��@�L@u%@\�@5?@-@O@�@ԕ@p�@T�@:�@#�@	l@�@Ɇ@�@j@D�@��@�@l�@]�@>�@��@�<@�F@��@}V@p;@J�@4@ϫ@��@O�@+�@�@�	@��@�U@��@h�@/�@1@ݘ@��@dZ@1�@$t@"�@"�@S@
ȴ@
��@
��@
z@
p;@
+k@
�@

�@	�=@	p�@	c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
B
9B
B
9B
9B
9B
B
9B
mB
mB
mB
SB
9B
�B
�B
�B
$B
�B
�B
YB
�B
�B
yB
�B
1B
B
!�B
%�B
0;B
QNB
[=B
\CB
[�B
e�B
UgB
��B
��B
�B
�DBB]/B��BMB �B%�B3�B@B4�B<B<PB7�B0�B)*BYB�B�B�B�B�MB �B��B�8B�bB�'B��B��B`vB>wB+kB)�B$�BEB
��B
��B
�B
�B
��B
f�B
N�B
G�B
,�B
�B	�yB	ΊB	ŢB	�B	�`B	��B	�B	��B	�oB	��B	zB	g�B	]/B	O�B	DgB	>�B	8B	2�B	)�B	�B	+B	�B�wB��B�B��B��B�8B�GB�B	�B	 iB��BۦBӏB�9B�B��B�2B�hB�'B�B�B�5B�5B�|B�tB�B	B	jB	�B	MB	VB	%B	)�B	.�B	-)B	-wB	4�B	CGB	OvB	Q�B	m�B	vB	��B	��B	i�B	R�B	G�B	F�B	<B	B�B	C�B	FYB	I�B	M�B	VB	d�B	uB	n�B	e�B	c�B	cnB	f�B	l�B	�,B	��B	��B	��B	��B	��B	��B	�&B	�UB	�B	�B	�iB	��B	�B	�FB	�B	��B	�+B	��B	��B	��B	��B	�WB	�IB	�WB	��B	��B	�vB	��B	��B	�B	��B	�B	��B	��B	�`B	��B	��B	��B	�B	�<B	�BB	��B	��B	��B	��B	�HB	��B	��B	� B	�;B	� B	� B	��B	��B	� B	�}B	�BB	�B	��B	�JB	��B	�0B	�0B	�JB	��B	��B	�B	��B	�VB	��B	ªB	�B	��B	��B	��B	ǔB	��B	��B	��B	ʦB	��B	��B	�_B	�B	�jB	�VB	�PB	�PB	�BB	ԯB	�EB	�xB	�qB	��B	�B	�!B	� B	�LB	�sB	�B	��B	��B	�`B	��B	�B	�hB	��B	�B	�B	��B	��B	�B	�B	��B	�:B	�hB	��B	�B	�B	�OB	�B	�OB	��B	��B	�aB	�B	�B	�GB	�B	�B	��B	�B	�}B	� B	�B	��B	�B	��B	��B	�OB	�B	�;B	��B	�vB	�B	�B	�aB	�|B	�GB	�GB	�GB	�-B	��B	��B	�?B	�ZB	�B	��B	�?B	��B	�fB	�8B	�LB	�0B	�JB	�>B	��B	�rB	�$B	��B	��B	�B	�B	�0B	�B	�B	�PB	��B	�B	��B	��B	��B	��B	�cB	�cB	��B
 �B
 �B
 �B
 �B
 �B	�cB	��B	��B
 4B	��B	��B	�cB
 B
 4B
 iB
 iB
 �B
B
�B
�B
%B
tB
�B
�B
B
EB
_B
�B
KB
�B
+B
_B
�B
�B
�B
fB

�B

�B

�B

XB

rB

rB

�B

=B

	B

	B

	B

�B
)B
^B
xB
0B
dB
�B
�B
�B
jB
B
HB
}B
bB
HB
B
�B
�B
�B
vB
B
�B
B
HB
�B
�B
�B
�B
�B
�B
FB
�B
B
B
mB
�B
�B
�B
B
�B
�B
�B
?B
YB
?B
�B
�B
�B
uB
�B
&B
�B
,B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
�B
�B
7B
B
=B
�B
�B
EB
�B
KB
kB
QB
B
B
7B
B
B
�B
WB
�B
�B
�B
B
�B
!B
B
jB
�B
�B
OB
5B
OB
�B
;B
!B
!B
VB
�B
 vB
 �B
!|B
!HB
!HB
!�B
!�B
!�B
"4B
"�B
#B
# B
$B
$@B
$�B
$�B
%,B
%�B
&fB
&�B
&�B
'B
'�B
(>B
(>B
($B
(>B
(XB
(�B
(�B
)DB
)yB
)�B
*0B
*�B
*�B
*�B
+kB
+�B
+�B
+�B
,�B
,�B
-]B
-�B
-�B
-�B
.cB
/ B
.�B
/�B
0oB
0�B
0�B
1'B
1AB
1AB
1[B
1�B
2aB
2�B
33B
3B
4B
5B
4�B
4�B
4�B
5�B
6+B
6B
6zB
5�B
6�B
7LB
5�B
6�B
7LB
72B
6�B
7B
7LB
7�B
:�B
;�B
<B
<�B
<�B
=<B
=�B
=�B
>BB
>�B
?.B
?}B
?cB
?cB
?�B
@�B
@�B
@OB
?�B
?B
?cB
@�B
@�B
A B
B'B
BAB
A�B
A�B
B[B
C-B
DMB
D�B
D�B
D�B
EB
EB
EmB
EmB
EmB
FB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J	B
J=B
JrB
JrB
J�B
J�B
J�B
KDB
KDB
K�B
K^B
KxB
L0B
L~B
LdB
L�B
L~B
L�B
M6B
M�B
N<B
N<B
N<B
N<B
NB
N�B
OB
O\B
OvB
O�B
P}B
Q B
QNB
Q�B
Q�B
R B
R�B
R�B
R�B
S[B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
UB
T�B
U�B
U�B
U�B
VB
VB
VB
V9B
V�B
V�B
W$B
WYB
WsB
W�B
W�B
XB
X+B
X�B
X�B
Y�B
ZB
ZB
Y�B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[qB
[�B
[�B
\B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
^jB
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`'B
`\B
`\B
`�B
`�B
aB
aB
`�B
abB
aHB
abB
a�B
a|B
a�B
a�B
a�B
bB
bB
b�B
b�B
b�B
cTB
c�B
c�B
cnB
dtB
dZB
d�B
d�B
e`B
e`B
e�B
e�B
e�B
gB
gB
g�B
g�B
g�B
h$B
h>B
hsB
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
jKB
jKB
j�B
j�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mB
mB
m]B
m]B
mwB
m�B
nB
nB
n}B
n�B
oOB
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
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
t9B
t9B
t9B
t�B
t�B
t�B
uZB
utB
uZB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
vzB
v�B
vzB
v�B
v�B
v�B
wB
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
y�B
zB
zxB
z�B
z^B
z�B
z�B
z�B
{JB
{�B
{B
{�B
|B
|jB
|�B
|�B
}B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~BB
~BB
~]B
~]B
~�B
.B
HB
HB
HB
}B
�B
�B
� B
�B
�4B
��B
��B
� B
� B
� B
�UB
��B
��B
��B
��B
��B
��B
�'B
�AB
��B
��B
��B
��B
�GB
�aB
�GB
�aB
��B
��B
��B
�3B
��B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�%B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
B
9B
B
9B
9B
9B
B
9B
mB
mB
mB
SB
9B
�B
�B
�B
$B
�B
�B
YB
�B
�B
yB
�B
1B
B
!�B
%�B
0;B
QNB
[=B
\CB
[�B
e�B
UgB
��B
��B
�B
�DBB]/B��BMB �B%�B3�B@B4�B<B<PB7�B0�B)*BYB�B�B�B�B�MB �B��B�8B�bB�'B��B��B`vB>wB+kB)�B$�BEB
��B
��B
�B
�B
��B
f�B
N�B
G�B
,�B
�B	�yB	ΊB	ŢB	�B	�`B	��B	�B	��B	�oB	��B	zB	g�B	]/B	O�B	DgB	>�B	8B	2�B	)�B	�B	+B	�B�wB��B�B��B��B�8B�GB�B	�B	 iB��BۦBӏB�9B�B��B�2B�hB�'B�B�B�5B�5B�|B�tB�B	B	jB	�B	MB	VB	%B	)�B	.�B	-)B	-wB	4�B	CGB	OvB	Q�B	m�B	vB	��B	��B	i�B	R�B	G�B	F�B	<B	B�B	C�B	FYB	I�B	M�B	VB	d�B	uB	n�B	e�B	c�B	cnB	f�B	l�B	�,B	��B	��B	��B	��B	��B	��B	�&B	�UB	�B	�B	�iB	��B	�B	�FB	�B	��B	�+B	��B	��B	��B	��B	�WB	�IB	�WB	��B	��B	�vB	��B	��B	�B	��B	�B	��B	��B	�`B	��B	��B	��B	�B	�<B	�BB	��B	��B	��B	��B	�HB	��B	��B	� B	�;B	� B	� B	��B	��B	� B	�}B	�BB	�B	��B	�JB	��B	�0B	�0B	�JB	��B	��B	�B	��B	�VB	��B	ªB	�B	��B	��B	��B	ǔB	��B	��B	��B	ʦB	��B	��B	�_B	�B	�jB	�VB	�PB	�PB	�BB	ԯB	�EB	�xB	�qB	��B	�B	�!B	� B	�LB	�sB	�B	��B	��B	�`B	��B	�B	�hB	��B	�B	�B	��B	��B	�B	�B	��B	�:B	�hB	��B	�B	�B	�OB	�B	�OB	��B	��B	�aB	�B	�B	�GB	�B	�B	��B	�B	�}B	� B	�B	��B	�B	��B	��B	�OB	�B	�;B	��B	�vB	�B	�B	�aB	�|B	�GB	�GB	�GB	�-B	��B	��B	�?B	�ZB	�B	��B	�?B	��B	�fB	�8B	�LB	�0B	�JB	�>B	��B	�rB	�$B	��B	��B	�B	�B	�0B	�B	�B	�PB	��B	�B	��B	��B	��B	��B	�cB	�cB	��B
 �B
 �B
 �B
 �B
 �B	�cB	��B	��B
 4B	��B	��B	�cB
 B
 4B
 iB
 iB
 �B
B
�B
�B
%B
tB
�B
�B
B
EB
_B
�B
KB
�B
+B
_B
�B
�B
�B
fB

�B

�B

�B

XB

rB

rB

�B

=B

	B

	B

	B

�B
)B
^B
xB
0B
dB
�B
�B
�B
jB
B
HB
}B
bB
HB
B
�B
�B
�B
vB
B
�B
B
HB
�B
�B
�B
�B
�B
�B
FB
�B
B
B
mB
�B
�B
�B
B
�B
�B
�B
?B
YB
?B
�B
�B
�B
uB
�B
&B
�B
,B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
�B
�B
7B
B
=B
�B
�B
EB
�B
KB
kB
QB
B
B
7B
B
B
�B
WB
�B
�B
�B
B
�B
!B
B
jB
�B
�B
OB
5B
OB
�B
;B
!B
!B
VB
�B
 vB
 �B
!|B
!HB
!HB
!�B
!�B
!�B
"4B
"�B
#B
# B
$B
$@B
$�B
$�B
%,B
%�B
&fB
&�B
&�B
'B
'�B
(>B
(>B
($B
(>B
(XB
(�B
(�B
)DB
)yB
)�B
*0B
*�B
*�B
*�B
+kB
+�B
+�B
+�B
,�B
,�B
-]B
-�B
-�B
-�B
.cB
/ B
.�B
/�B
0oB
0�B
0�B
1'B
1AB
1AB
1[B
1�B
2aB
2�B
33B
3B
4B
5B
4�B
4�B
4�B
5�B
6+B
6B
6zB
5�B
6�B
7LB
5�B
6�B
7LB
72B
6�B
7B
7LB
7�B
:�B
;�B
<B
<�B
<�B
=<B
=�B
=�B
>BB
>�B
?.B
?}B
?cB
?cB
?�B
@�B
@�B
@OB
?�B
?B
?cB
@�B
@�B
A B
B'B
BAB
A�B
A�B
B[B
C-B
DMB
D�B
D�B
D�B
EB
EB
EmB
EmB
EmB
FB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J	B
J=B
JrB
JrB
J�B
J�B
J�B
KDB
KDB
K�B
K^B
KxB
L0B
L~B
LdB
L�B
L~B
L�B
M6B
M�B
N<B
N<B
N<B
N<B
NB
N�B
OB
O\B
OvB
O�B
P}B
Q B
QNB
Q�B
Q�B
R B
R�B
R�B
R�B
S[B
S�B
S�B
T,B
TFB
T�B
T�B
T�B
UB
T�B
U�B
U�B
U�B
VB
VB
VB
V9B
V�B
V�B
W$B
WYB
WsB
W�B
W�B
XB
X+B
X�B
X�B
Y�B
ZB
ZB
Y�B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[qB
[�B
[�B
\B
\�B
\�B
\�B
]IB
]~B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
^jB
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`'B
`\B
`\B
`�B
`�B
aB
aB
`�B
abB
aHB
abB
a�B
a|B
a�B
a�B
a�B
bB
bB
b�B
b�B
b�B
cTB
c�B
c�B
cnB
dtB
dZB
d�B
d�B
e`B
e`B
e�B
e�B
e�B
gB
gB
g�B
g�B
g�B
h$B
h>B
hsB
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
jKB
jKB
j�B
j�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mB
mB
m]B
m]B
mwB
m�B
nB
nB
n}B
n�B
oOB
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
q'B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
t9B
t9B
t9B
t�B
t�B
t�B
uZB
utB
uZB
u�B
u�B
u�B
u�B
u�B
vB
v+B
v`B
vzB
v�B
vzB
v�B
v�B
v�B
wB
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
y�B
zB
zxB
z�B
z^B
z�B
z�B
z�B
{JB
{�B
{B
{�B
|B
|jB
|�B
|�B
}B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
~BB
~BB
~]B
~]B
~�B
.B
HB
HB
HB
}B
�B
�B
� B
�B
�4B
��B
��B
� B
� B
� B
�UB
��B
��B
��B
��B
��B
��B
�'B
�AB
��B
��B
��B
��B
�GB
�aB
�GB
�aB
��B
��B
��B
�3B
��B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�%B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221104065045  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221104065047  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221104065048  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221104065048                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221104155053  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221104155053  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221104070739                      G�O�G�O�G�O�                