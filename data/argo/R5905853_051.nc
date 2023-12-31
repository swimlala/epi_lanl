CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:31:48Z creation;2022-06-04T17:31:49Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173148  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�$u��,`1   @�$v3���@/�;dZ��c�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B�  B�  B�  B�  B�  Bۙ�B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C�fC�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*� C+� C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR33CT�CU�fCX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@}p�@��R@��RA ��A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��RB��RB��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��BۅB��B��B��B��B��B��B��B��B��C��C��C]C��C	��C��C��C�)C�)C��C��C��C��C��C��C��C!��C#��C%��C'��C*u�C+u�C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CP]CR(�CT]CU�)CW��CY��C[��C]�)C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz��Dz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�A�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�u�A�t�A�uZA�sMA�qAA�s�A�t�A�w�A�p;A�n�A�e�A�^5A�\)A�T�A�QA�;dA�VA���A��NAΖ�A�xA�h>A�a�A�[�A�Z�A�Z�A�[#A�T�A�P�A�M6A�I�A�G�A�G�A�?}A�>A��A� 4A��6AƏ(A�XEA��3A��Aë�AA��{A���A�S[A��WA��tA�C�A�ŢA���A�}VA�oA�
�A���A��A�k�A� �A��gA��	A�?}A���A��A�xA��CA�bA�_A���A��A��"A�{�A�l�A��A�(�A�0�A���A��~A��:A��A��A�gA�o�A�A}��A{ݘAr�"An�VAm�aAl�Aj��Ai1�Ah��AfI�Ab� A^hsAZ~�AW7LAR�AN��AL��AK/�AH)�AG!�AG>BAFiDAEaAC��AA�AA�PA?��A;�A<�A<�5A>[WA>��A>��A<�A:-A8JA7��A7IRA6M�A5�A45?A2m�A1�A0��A/�A.��A-]dA,�A,��A,s�A+��A+��A+	A*�A*F�A*JA)I�A(�?A(�wA(o A&�oA%҉A%)�A$�A$]�A#�,A#y�A#S�A"�A"I�A!�]A!�A!��A!xlA!]dA!VA �*A �AZA�FA[WA-�A��A�A��A�A~(A�A(�A��Au�A��AcAMA�AE9A*0A��A/�A�A-A�AA	�A��A�Av�A��AiDA�ZA��A��A4�A�A�A�&A�.A�Ac�AZ�A!-A�BA��A
�A�A-A�A�}A|A]�A+kA�KAA�A͟A��Au%A�A
�A	��A	��A	��A	w�A	H�A�}A:�A�HAM�A�A �A�qA�A�nAc A�;A��AP�A4nAc�A��AQ�A�AɆAw�AK�A)�A��A($A��A�A�bA(�A �nA <6A �@�k�@���@��@�q�@���@�qv@��@���@���@�X@�33@��@���@�zx@�Y@���@�33@� \@��@��W@��@�v`@�C�@�#�@��|@���@�@�s@��@��@�?@� �@��@��@��N@���@��)@��@�8�@�O�@�s@㐗@�O@�Ĝ@�z@��&@�j�@��@���@�h
@��@ނA@�N�@�
=@�xl@�m�@�E�@�@۞�@ڲ�@�@�qv@�&@ج�@�Z@�d�@�v�@�J�@��@׉7@�O@� i@�:�@՘�@�8�@��'@�v�@��&@ӄM@ӆ�@�@�s@�=@�ȴ@҄�@�:*@Ѫ�@�e,@��@з�@�Z�@�b@��z@ώ"@�Y@���@Έ�@��]@̹$@�	@��@˞�@�j@�C�@�(�@��f@��5@��@��@ʦL@�0U@ɨX@�=@���@�&�@�s�@�V�@��@���@��g@�e,@ư!@�~@Śk@�5�@�O@��@�e�@���@�@�u@�H�@���@�~@�5�@��@�A�@���@�&�@�_p@��@�tT@�8�@��@�@�2�@�O@���@��@���@�<�@���@��@��n@��@�<�@���@�$t@�oi@��N@�O�@�V@��!@�u�@� �@�>�@��u@��m@�a@���@�l"@�W�@�?@�u@��@�X�@���@���@���@�&�@��#@���@�=@��[@�q�@�M@�ƨ@�\)@�2a@�j�@�33@��9@�/�@��@�?}@�ѷ@�YK@�$�@�1@��@�$t@�@���@���@�v�@�9X@��@��@���@�Y�@���@�g8@�~@�	�@��#@��@�a�@��@��@�ȴ@���@�m�@�4n@�1@���@��@�Vm@��@���@�{@���@��7@�T�@�1�@��@���@���@�J�@��@�˒@���@�c@�9�@���@��@��m@��z@��'@�j�@��@�ں@���@�y>@�G@��X@�l�@�!�@��@�r�@�ϫ@��f@�@O@��@���@�y>@���@���@�`B@�6z@�K�@���@�z�@���@��@��@��C@�qv@��P@�d�@�#:@��d@��@���@���@��D@�YK@��V@��2@���@���@���@���@��r@�d�@�7@��}@���@���@�'�@���@���@��@���@�V�@��@��@��^@��w@��@��M@�8@���@���@��,@���@��+@�]d@�B[@�1�@� �@��[@�n/@�.I@�;@��@���@�/�@��Q@���@�@O@�@���@��z@�tT@� �@�ϫ@�ƨ@��@��n@�~�@�Z�@�+�@��5@��6@��r@�^5@�:�@��@��@���@�T�@�&@���@�>B@��@U�@~��@~�@}[W@|�[@|e�@{��@{t�@z�"@z��@zW�@y��@x��@xu�@x-�@w�@w��@w@v�F@u�@u@uF@t�?@toi@t�@s�k@sg�@r�2@r��@rQ@q�@q[W@p�.@o��@o��@o]�@o�@n��@nq�@n=q@m�.@m�'@m�@l/�@k��@k�4@k6z@j��@ji�@j:*@j4@hbN@g��@g4�@f�@f~�@f�@e��@d�5@d֡@d�[@d�[@d�4@dQ�@d6@d!@d1@c��@c��@b�@b�1@b8�@a�@a�@ao @a&�@`��@`c�@`:�@_�&@_;d@^�@^�X@^��@^�@]�h@]`B@\�@\r�@[��@[�*@[|�@[C@Z�y@Z��@ZV@Y�o@Y��@Y��@Yf�@Y	l@X�K@XXy@W�@W��@W�@W�@V��@V�y@V�'@U��@U�h@UT�@T�@T��@Tz�@TN�@T@S�*@Sqv@SA�@S'�@S�@R�B@Rq�@RH�@R!�@Q�@Q�@Q�@Qo @P�	@P�e@P4n@O�;@OS�@O�@O�@N�@N�h@N?@M�H@M�@Mm]@Ma�@M%F@L�@LM@K�@@K.I@K�@K�@J�@J��@JR�@J4@I��@I��@I��@IVm@H�@H��@HU2@H  @G��@G&@F��@F��@Fn�@F8�@E��@E��@D�?@D�@Dg8@D>B@D �@D4n@D�@C��@C!-@B~�@B@A�>@A^�@@�@@Ɇ@@��@@A�@@1@@	�@@�@@x@@�@?��@?+@>��@>_�@>u@=��@=0�@<�@<Ft@;�@;�@@;t�@:�@:��@:��@:�r@:	@9j@9q@8Ft@8@7�@7x@7U�@6�!@66�@5��@5�=@5?}@4�K@4��@4��@4z�@4$@4�@3�@3��@3�:@3K�@3�@2ں@2�@2\�@2@�@1�d@1A @1�@0�9@0Xy@01@/ݘ@/�w@/��@/b�@/"�@.�@.z@.�@-��@-m]@-A @,��@,�@,��@,��@,1'@+� @+��@+\)@+o@*��@*͟@*��@*��@*xl@*:*@)ԕ@)��@)O�@(�f@(�)@(��@(�@(r�@(9X@'��@'�{@&ں@&xl@&Z�@&8�@%�9@%}�@%2a@%%@%�@%�@$�K@$Ĝ@$�e@$oi@$6@$@#�}@#�@#Mj@#8@#.I@#�@"�y@"�@"i�@"W�@"ff@"d�@"&�@!��@!��@!c�@!*0@!	l@ �@ �o@ e�@ %�@�@�@�q@x@J#@�@��@��@��@)�@�Z@�9@�h@\�@!�@�@��@��@[�@H@x@�[@U�@�@��@.�@�@p�@O�@A @?}@�P@�@bN@'R@x@��@s@�@�]@�6@&�@@	@�)@\�@4@*0@@@�U@z�@9X@@��@�8@҉@��@��@s�@=q@�@��@��@?}@�@��@H@�r@�@خ@�	@g�@C�@�@��@~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�u�A�t�A�uZA�sMA�qAA�s�A�t�A�w�A�p;A�n�A�e�A�^5A�\)A�T�A�QA�;dA�VA���A��NAΖ�A�xA�h>A�a�A�[�A�Z�A�Z�A�[#A�T�A�P�A�M6A�I�A�G�A�G�A�?}A�>A��A� 4A��6AƏ(A�XEA��3A��Aë�AA��{A���A�S[A��WA��tA�C�A�ŢA���A�}VA�oA�
�A���A��A�k�A� �A��gA��	A�?}A���A��A�xA��CA�bA�_A���A��A��"A�{�A�l�A��A�(�A�0�A���A��~A��:A��A��A�gA�o�A�A}��A{ݘAr�"An�VAm�aAl�Aj��Ai1�Ah��AfI�Ab� A^hsAZ~�AW7LAR�AN��AL��AK/�AH)�AG!�AG>BAFiDAEaAC��AA�AA�PA?��A;�A<�A<�5A>[WA>��A>��A<�A:-A8JA7��A7IRA6M�A5�A45?A2m�A1�A0��A/�A.��A-]dA,�A,��A,s�A+��A+��A+	A*�A*F�A*JA)I�A(�?A(�wA(o A&�oA%҉A%)�A$�A$]�A#�,A#y�A#S�A"�A"I�A!�]A!�A!��A!xlA!]dA!VA �*A �AZA�FA[WA-�A��A�A��A�A~(A�A(�A��Au�A��AcAMA�AE9A*0A��A/�A�A-A�AA	�A��A�Av�A��AiDA�ZA��A��A4�A�A�A�&A�.A�Ac�AZ�A!-A�BA��A
�A�A-A�A�}A|A]�A+kA�KAA�A͟A��Au%A�A
�A	��A	��A	��A	w�A	H�A�}A:�A�HAM�A�A �A�qA�A�nAc A�;A��AP�A4nAc�A��AQ�A�AɆAw�AK�A)�A��A($A��A�A�bA(�A �nA <6A �@�k�@���@��@�q�@���@�qv@��@���@���@�X@�33@��@���@�zx@�Y@���@�33@� \@��@��W@��@�v`@�C�@�#�@��|@���@�@�s@��@��@�?@� �@��@��@��N@���@��)@��@�8�@�O�@�s@㐗@�O@�Ĝ@�z@��&@�j�@��@���@�h
@��@ނA@�N�@�
=@�xl@�m�@�E�@�@۞�@ڲ�@�@�qv@�&@ج�@�Z@�d�@�v�@�J�@��@׉7@�O@� i@�:�@՘�@�8�@��'@�v�@��&@ӄM@ӆ�@�@�s@�=@�ȴ@҄�@�:*@Ѫ�@�e,@��@з�@�Z�@�b@��z@ώ"@�Y@���@Έ�@��]@̹$@�	@��@˞�@�j@�C�@�(�@��f@��5@��@��@ʦL@�0U@ɨX@�=@���@�&�@�s�@�V�@��@���@��g@�e,@ư!@�~@Śk@�5�@�O@��@�e�@���@�@�u@�H�@���@�~@�5�@��@�A�@���@�&�@�_p@��@�tT@�8�@��@�@�2�@�O@���@��@���@�<�@���@��@��n@��@�<�@���@�$t@�oi@��N@�O�@�V@��!@�u�@� �@�>�@��u@��m@�a@���@�l"@�W�@�?@�u@��@�X�@���@���@���@�&�@��#@���@�=@��[@�q�@�M@�ƨ@�\)@�2a@�j�@�33@��9@�/�@��@�?}@�ѷ@�YK@�$�@�1@��@�$t@�@���@���@�v�@�9X@��@��@���@�Y�@���@�g8@�~@�	�@��#@��@�a�@��@��@�ȴ@���@�m�@�4n@�1@���@��@�Vm@��@���@�{@���@��7@�T�@�1�@��@���@���@�J�@��@�˒@���@�c@�9�@���@��@��m@��z@��'@�j�@��@�ں@���@�y>@�G@��X@�l�@�!�@��@�r�@�ϫ@��f@�@O@��@���@�y>@���@���@�`B@�6z@�K�@���@�z�@���@��@��@��C@�qv@��P@�d�@�#:@��d@��@���@���@��D@�YK@��V@��2@���@���@���@���@��r@�d�@�7@��}@���@���@�'�@���@���@��@���@�V�@��@��@��^@��w@��@��M@�8@���@���@��,@���@��+@�]d@�B[@�1�@� �@��[@�n/@�.I@�;@��@���@�/�@��Q@���@�@O@�@���@��z@�tT@� �@�ϫ@�ƨ@��@��n@�~�@�Z�@�+�@��5@��6@��r@�^5@�:�@��@��@���@�T�@�&@���@�>B@��@U�@~��@~�@}[W@|�[@|e�@{��@{t�@z�"@z��@zW�@y��@x��@xu�@x-�@w�@w��@w@v�F@u�@u@uF@t�?@toi@t�@s�k@sg�@r�2@r��@rQ@q�@q[W@p�.@o��@o��@o]�@o�@n��@nq�@n=q@m�.@m�'@m�@l/�@k��@k�4@k6z@j��@ji�@j:*@j4@hbN@g��@g4�@f�@f~�@f�@e��@d�5@d֡@d�[@d�[@d�4@dQ�@d6@d!@d1@c��@c��@b�@b�1@b8�@a�@a�@ao @a&�@`��@`c�@`:�@_�&@_;d@^�@^�X@^��@^�@]�h@]`B@\�@\r�@[��@[�*@[|�@[C@Z�y@Z��@ZV@Y�o@Y��@Y��@Yf�@Y	l@X�K@XXy@W�@W��@W�@W�@V��@V�y@V�'@U��@U�h@UT�@T�@T��@Tz�@TN�@T@S�*@Sqv@SA�@S'�@S�@R�B@Rq�@RH�@R!�@Q�@Q�@Q�@Qo @P�	@P�e@P4n@O�;@OS�@O�@O�@N�@N�h@N?@M�H@M�@Mm]@Ma�@M%F@L�@LM@K�@@K.I@K�@K�@J�@J��@JR�@J4@I��@I��@I��@IVm@H�@H��@HU2@H  @G��@G&@F��@F��@Fn�@F8�@E��@E��@D�?@D�@Dg8@D>B@D �@D4n@D�@C��@C!-@B~�@B@A�>@A^�@@�@@Ɇ@@��@@A�@@1@@	�@@�@@x@@�@?��@?+@>��@>_�@>u@=��@=0�@<�@<Ft@;�@;�@@;t�@:�@:��@:��@:�r@:	@9j@9q@8Ft@8@7�@7x@7U�@6�!@66�@5��@5�=@5?}@4�K@4��@4��@4z�@4$@4�@3�@3��@3�:@3K�@3�@2ں@2�@2\�@2@�@1�d@1A @1�@0�9@0Xy@01@/ݘ@/�w@/��@/b�@/"�@.�@.z@.�@-��@-m]@-A @,��@,�@,��@,��@,1'@+� @+��@+\)@+o@*��@*͟@*��@*��@*xl@*:*@)ԕ@)��@)O�@(�f@(�)@(��@(�@(r�@(9X@'��@'�{@&ں@&xl@&Z�@&8�@%�9@%}�@%2a@%%@%�@%�@$�K@$Ĝ@$�e@$oi@$6@$@#�}@#�@#Mj@#8@#.I@#�@"�y@"�@"i�@"W�@"ff@"d�@"&�@!��@!��@!c�@!*0@!	l@ �@ �o@ e�@ %�@�@�@�q@x@J#@�@��@��@��@)�@�Z@�9@�h@\�@!�@�@��@��@[�@H@x@�[@U�@�@��@.�@�@p�@O�@A @?}@�P@�@bN@'R@x@��@s@�@�]@�6@&�@@	@�)@\�@4@*0@@@�U@z�@9X@@��@�8@҉@��@��@s�@=q@�@��@��@?}@�@��@H@�r@�@خ@�	@g�@C�@�@��@~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	I7B	IB	IlB	IlB	IlB	IRB	I�B	I�B	IlB	IRB	I7B	H�B	H�B	G�B	GEB	E�B	C-B	@B	="B	9�B	7�B	6�B	6�B	6�B	6�B	6�B	7B	7B	6�B	6�B	6�B	6�B	6`B	5%B	$�B	 �B	�B	�B	 'B	+6B	:^B	H�B	U�B	S[B	e`B	�xB	��B
(�B
p�B
�'B
�CB
ðB�B'B8�B�B�0B��B�B�@B�B��B�B�
B�zB�B��BՁB�B��B��B�'BY�BAB5?B,"BsB�B
�TB
�9B
��B
�vB
U�B
�B	�B	��B	]dB	EmB	AB	=<B	8B	/�B	<�B	G�B	6FB	"hB	 B	9B��B�B��B�/B�]B��B	�B	-)B	H�B	M6B	`\B	�RB	��B	��B	ԕB
~B
HB
MPB
L�B
E�B
6FB
'mB
-�B
6`B
3�B
0!B
.�B
&�B
�B
�B
�B
�B

B
#�B
'B
)�B
'�B
*B
+6B
-B
3�B
6�B
8�B
6FB
6�B
4�B
.�B
'�B
#:B
!�B
!�B
&LB
'�B
)B
)�B
,qB
1�B
4TB
="B
@iB
A�B
F%B
G�B
I�B
J	B
K^B
KDB
K�B
KB
D�B
=�B
72B
3�B
0�B
-CB
-wB
2|B
/�B
5B
;dB
="B
=B
<�B
;B
<6B
<6B
;0B
>BB
7�B
,WB
,�B
0!B
*�B
-�B
)yB
(sB
'RB
%�B
%�B
8lB
MB
QB
UB
VB
YeB
Y�B
Y�B
X�B
W�B
VB
S�B
SB
R�B
Q�B
RB
Q B
N�B
MB
I�B
K)B
JXB
IlB
CGB
BB
FB
E�B
D�B
DB
B�B
?cB
AB
?�B
@�B
?�B
=�B
;dB
>BB
?}B
=�B
>]B
<PB
<B
?�B
D�B
D�B
DB
B�B
A�B
@�B
@4B
>�B
=<B
;B
="B
=VB
<B
;�B
:^B
9XB
8B
5�B
5B
2|B
1B
0B
/ B
,qB
($B
$�B
$@B
$�B
 �B
	B
�B
+B
�B
7B
B
�B
�B
1B
SB
aB
�B
�B
�B
�B
mB
�B
�B
�B
�B
SB
�B
{B
hB
�B
�B
�B
B
�B
 B
�B
 B
NB
}B
B
�B
.B
B

	B

rB
�B
MB
�B
tB
B
?B
SB
aB
�B
�B
[B
�B
aB
�B
�B
YB
%B
%B
�B
SB
�B
�B
 �B	��B	�HB	�.B
 B
EB
YB
YB
tB
�B
�B
�B
�B
B
9B
9B
B
9B
B
�B
mB
B
�B
�B
UB
 �B
 �B
 �B
 �B
 �B
�B
�B
MB
B
�B
�B
�B
�B
{B
GB
EB
�B
�B
�B
�B
�B
�B
EB
�B
	RB
	RB
	�B

�B

�B
	�B
	lB
�B
�B
�B
�B
uB
-B
B
�B
	�B

#B
	�B
	�B
	lB
�B
3B
B
�B
�B
�B
9B
zB
fB
	lB
�B
	B
�B
	�B

=B

	B

�B

�B

#B
	�B
	B
�B
_B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
fB
	�B
	lB
	�B
	�B

�B

�B

�B
B
�B
B
4B
NB
 B
�B
"B
�B
"B
VB
<B
B
�B
�B
 B
�B
�B
NB
B
NB
�B
�B
�B
gB
B
�B
�B
SB
mB
�B
�B
�B
�B
B
B
mB
�B
�B
�B

B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
eB
�B
B
�B
�B
B
�B
WB
WB
qB
qB
�B
�B
CB
]B
]B
�B
�B
xB
]B
B
xB
�B
dB
/B
dB
�B
�B
B
WB
#B
/B
 �B
 �B
pB
B
�B
 B
#TB
$�B
$�B
# B
"�B
"�B
"B
!�B
!�B
"�B
$�B
%FB
$�B
%�B
&LB
'B
(�B
(�B
)B
)_B
)B
(�B
)DB
)yB
(XB
(�B
)DB
)�B
*eB
*B
*KB
*�B
+�B
,B
,�B
,�B
-B
-)B
,�B
-)B
-)B
-B
-B
,�B
-CB
-]B
-�B
./B
.IB
.cB
.cB
.IB
.cB
.�B
/OB
/5B
/OB
/�B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1vB
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
4B
4�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
6zB
6FB
6FB
6FB
6FB
6�B
7B
7�B
7fB
7�B
8B
8B
88B
8lB
8lB
8�B
8�B
8�B
9$B
9XB
9�B
:^B
:�B
:�B
:�B
;JB
;dB
;dB
;�B
;�B
<jB
<�B
="B
=qB
=VB
<�B
<�B
<�B
<�B
<B
;�B
;�B
;�B
;�B
<B
=<B
>(B
>B
>B
=�B
>�B
?�B
@ B
@OB
@�B
@�B
@�B
@4B
@�B
AB
A�B
B�B
B�B
CGB
C{B
C�B
D3B
D3B
DMB
DgB
DgB
DgB
D�B
D�B
FYB
G+B
GEB
GEB
G�B
HfB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J#B
J=B
J=B
J�B
J�B
J�B
KB
J�B
KB
KB
KB
LB
L0B
LJB
LdB
L~B
L~B
LdB
L�B
L�B
MB
M6B
M6B
MPB
M�B
NB
NB
N<B
NVB
N"B
N<B
N�B
N�B
N�B
OBB
O�B
O�B
PB
PB
PB
P.B
P�B
P�B
Q B
QB
QB
QhB
Q�B
R:B
R�B
R�B
R�B
R�B
R�B
S&B
S&B
S@B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
SuB
SuB
S[B
S�B
S[B
SuB
S[B
S[B
S�B
SuB
S�B
TB
T{B
UB
U�B
U�B
U�B
VB
U�B
U�B
VSB
VmB
V�B
W
B
W�B
XB
XB
XB
X+B
XEB
X�B
YKB
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
[	B
[#B
[WB
[WB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
]~B
]~B
]�B
]�B
]�B
^jB
^�B
^�B
_B
_pB
_�B
_�B
_�B
_�B
`'B
`B
`'B
`BB
`\B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
bB
bNB
bhB
bNB
b�B
b�B
cTB
c:B
c�B
c�B
c�B
dB
dZB
dZB
d�B
d�B
d�B
d�B
e`B
ezB
ezB
e�B
e�B
e�B
fB
f2B
f2B
fLB
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h
B
h$B
h
B
h
B
h
B
g�B
h$B
h
B
h>B
h�B
i*B
i_B
i�B
i�B
i�B
i�B
jB
j0B
jB
j0B
jKB
jeB
j�B
kB
k6B
k6B
kB
kkB
k�B
k�B
l"B
l=B
lWB
l�B
mB
m)B
m)B
mB
mB
mwB
m�B
mwB
m�B
m�B
m�B
n/B
ncB
n}B
o B
o B
o B
o B
oiB
oiB
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
p�B
qB
qAB
q�B
r-B
r-B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
zB
z*B
zxB
z�B
{0B
{dB
{B
{JB
{�B
|B
{�B
|PB
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	I7B	IB	IlB	IlB	IlB	IRB	I�B	I�B	IlB	IRB	I7B	H�B	H�B	G�B	GEB	E�B	C-B	@B	="B	9�B	7�B	6�B	6�B	6�B	6�B	6�B	7B	7B	6�B	6�B	6�B	6�B	6`B	5%B	$�B	 �B	�B	�B	 'B	+6B	:^B	H�B	U�B	S[B	e`B	�xB	��B
(�B
p�B
�'B
�CB
ðB�B'B8�B�B�0B��B�B�@B�B��B�B�
B�zB�B��BՁB�B��B��B�'BY�BAB5?B,"BsB�B
�TB
�9B
��B
�vB
U�B
�B	�B	��B	]dB	EmB	AB	=<B	8B	/�B	<�B	G�B	6FB	"hB	 B	9B��B�B��B�/B�]B��B	�B	-)B	H�B	M6B	`\B	�RB	��B	��B	ԕB
~B
HB
MPB
L�B
E�B
6FB
'mB
-�B
6`B
3�B
0!B
.�B
&�B
�B
�B
�B
�B

B
#�B
'B
)�B
'�B
*B
+6B
-B
3�B
6�B
8�B
6FB
6�B
4�B
.�B
'�B
#:B
!�B
!�B
&LB
'�B
)B
)�B
,qB
1�B
4TB
="B
@iB
A�B
F%B
G�B
I�B
J	B
K^B
KDB
K�B
KB
D�B
=�B
72B
3�B
0�B
-CB
-wB
2|B
/�B
5B
;dB
="B
=B
<�B
;B
<6B
<6B
;0B
>BB
7�B
,WB
,�B
0!B
*�B
-�B
)yB
(sB
'RB
%�B
%�B
8lB
MB
QB
UB
VB
YeB
Y�B
Y�B
X�B
W�B
VB
S�B
SB
R�B
Q�B
RB
Q B
N�B
MB
I�B
K)B
JXB
IlB
CGB
BB
FB
E�B
D�B
DB
B�B
?cB
AB
?�B
@�B
?�B
=�B
;dB
>BB
?}B
=�B
>]B
<PB
<B
?�B
D�B
D�B
DB
B�B
A�B
@�B
@4B
>�B
=<B
;B
="B
=VB
<B
;�B
:^B
9XB
8B
5�B
5B
2|B
1B
0B
/ B
,qB
($B
$�B
$@B
$�B
 �B
	B
�B
+B
�B
7B
B
�B
�B
1B
SB
aB
�B
�B
�B
�B
mB
�B
�B
�B
�B
SB
�B
{B
hB
�B
�B
�B
B
�B
 B
�B
 B
NB
}B
B
�B
.B
B

	B

rB
�B
MB
�B
tB
B
?B
SB
aB
�B
�B
[B
�B
aB
�B
�B
YB
%B
%B
�B
SB
�B
�B
 �B	��B	�HB	�.B
 B
EB
YB
YB
tB
�B
�B
�B
�B
B
9B
9B
B
9B
B
�B
mB
B
�B
�B
UB
 �B
 �B
 �B
 �B
 �B
�B
�B
MB
B
�B
�B
�B
�B
{B
GB
EB
�B
�B
�B
�B
�B
�B
EB
�B
	RB
	RB
	�B

�B

�B
	�B
	lB
�B
�B
�B
�B
uB
-B
B
�B
	�B

#B
	�B
	�B
	lB
�B
3B
B
�B
�B
�B
9B
zB
fB
	lB
�B
	B
�B
	�B

=B

	B

�B

�B

#B
	�B
	B
�B
_B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
fB
	�B
	lB
	�B
	�B

�B

�B

�B
B
�B
B
4B
NB
 B
�B
"B
�B
"B
VB
<B
B
�B
�B
 B
�B
�B
NB
B
NB
�B
�B
�B
gB
B
�B
�B
SB
mB
�B
�B
�B
�B
B
B
mB
�B
�B
�B

B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
eB
�B
B
�B
�B
B
�B
WB
WB
qB
qB
�B
�B
CB
]B
]B
�B
�B
xB
]B
B
xB
�B
dB
/B
dB
�B
�B
B
WB
#B
/B
 �B
 �B
pB
B
�B
 B
#TB
$�B
$�B
# B
"�B
"�B
"B
!�B
!�B
"�B
$�B
%FB
$�B
%�B
&LB
'B
(�B
(�B
)B
)_B
)B
(�B
)DB
)yB
(XB
(�B
)DB
)�B
*eB
*B
*KB
*�B
+�B
,B
,�B
,�B
-B
-)B
,�B
-)B
-)B
-B
-B
,�B
-CB
-]B
-�B
./B
.IB
.cB
.cB
.IB
.cB
.�B
/OB
/5B
/OB
/�B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1vB
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
4B
4B
4�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
5?B
5�B
6zB
6FB
6FB
6FB
6FB
6�B
7B
7�B
7fB
7�B
8B
8B
88B
8lB
8lB
8�B
8�B
8�B
9$B
9XB
9�B
:^B
:�B
:�B
:�B
;JB
;dB
;dB
;�B
;�B
<jB
<�B
="B
=qB
=VB
<�B
<�B
<�B
<�B
<B
;�B
;�B
;�B
;�B
<B
=<B
>(B
>B
>B
=�B
>�B
?�B
@ B
@OB
@�B
@�B
@�B
@4B
@�B
AB
A�B
B�B
B�B
CGB
C{B
C�B
D3B
D3B
DMB
DgB
DgB
DgB
D�B
D�B
FYB
G+B
GEB
GEB
G�B
HfB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J#B
J=B
J=B
J�B
J�B
J�B
KB
J�B
KB
KB
KB
LB
L0B
LJB
LdB
L~B
L~B
LdB
L�B
L�B
MB
M6B
M6B
MPB
M�B
NB
NB
N<B
NVB
N"B
N<B
N�B
N�B
N�B
OBB
O�B
O�B
PB
PB
PB
P.B
P�B
P�B
Q B
QB
QB
QhB
Q�B
R:B
R�B
R�B
R�B
R�B
R�B
S&B
S&B
S@B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
SuB
SuB
S[B
S�B
S[B
SuB
S[B
S[B
S�B
SuB
S�B
TB
T{B
UB
U�B
U�B
U�B
VB
U�B
U�B
VSB
VmB
V�B
W
B
W�B
XB
XB
XB
X+B
XEB
X�B
YKB
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
[	B
[#B
[WB
[WB
[�B
[�B
[�B
[�B
\)B
\�B
\�B
]~B
]~B
]�B
]�B
]�B
^jB
^�B
^�B
_B
_pB
_�B
_�B
_�B
_�B
`'B
`B
`'B
`BB
`\B
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
bB
bNB
bhB
bNB
b�B
b�B
cTB
c:B
c�B
c�B
c�B
dB
dZB
dZB
d�B
d�B
d�B
d�B
e`B
ezB
ezB
e�B
e�B
e�B
fB
f2B
f2B
fLB
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h
B
h$B
h
B
h
B
h
B
g�B
h$B
h
B
h>B
h�B
i*B
i_B
i�B
i�B
i�B
i�B
jB
j0B
jB
j0B
jKB
jeB
j�B
kB
k6B
k6B
kB
kkB
k�B
k�B
l"B
l=B
lWB
l�B
mB
m)B
m)B
mB
mB
mwB
m�B
mwB
m�B
m�B
m�B
n/B
ncB
n}B
o B
o B
o B
o B
oiB
oiB
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
p�B
qB
qAB
q�B
r-B
r-B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
zB
z*B
zxB
z�B
{0B
{dB
{B
{JB
{�B
|B
{�B
|PB
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104904  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173148  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173148  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173149                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023156  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023156  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                