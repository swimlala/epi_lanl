CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:58Z creation;2022-06-04T19:19:58Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191958  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               6A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�-&tn�c1   @�-&�6�@-�dZ��cp9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@�  A   A!��AA��Aa��A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�33B���B���B�  B�33B뙚B�  B�  B�ffB���B���C  C  C  C  C
  C  CL�C�fC  C  C  C  C�C�C  C�fC!�fC$  C&  C(  C*�C,�C.�C/�fC2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��R@��RA ��A@��A`��A\)A��GA��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BP=pBX=pB_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B�Q�B��B��RB��B��B��B��B��B��B��B��B��B۸RB߸RB��B��B�B��B��B�Q�B��RB��RC��C��C��C��C	��C��CB�C�)C��C��C��C��C]C]C��C�)C!�)C#��C%��C'��C*]C,]C.]C/�)C1��C3��C5�)C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf]Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDN�DN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�eA��A��A��A�!�A�!�A��A��A�~A�.A�$A�-A�'�A�+6A�-�A�&�A�+�A�+kA�,�A�.}A�,�A�*eA�($A�$tA�+6A�.A�1A��yA�k�A��zA�T�A�?�A�6FA�.�A� 'A��A��A��A��A��8AϡbA�^�A��AȼAĺ�A�SA�o5A���A��<A�n�A�QA�k�A��7A�M�A��A�H�A�c�A�fA�'RA�҉A��nA�v�A�ܒA��^A���A��oA�@A�\A�xA��IA�~A�k�A���A��A�]dA��]A�B�A���A���A���A�ܒA�7�A�j�A���A�K^A|�#A{��Ax��At֡Ap�qAj�^AfߤAe��Ad�AbVmA`��A^��AZ��AX�AX(AW��AU=�AS�YAQ�XANOAK��AJ��AJ�<AHp;AE��ADz�AC��AB��AB;dA@�zA?S�A>OA<�'A;A9xlA7��A6��A4�A2_A0�"A-A(�MA&U2A%��A%{�A&$�A&0UA&A%HA%VA$ƨA$�+A$c A$MA#��A#D�A"h�A  iA�8AƨA��Au�A4A�WA��A~�AC�Av�A��A�:AA�.AhsA�AkQATaA�fA4AW?A>�A$tA)_A�bA��A��A��Ay>A+AE�A�A�A��A�XAv�A.�A��A�AI�A��A�A��A�fA��AxAb�A{A
QA	خA	1�A�FA�_A�1A��A��As�AH�A�A��A\�A6A�
A6zA�A�A�tA��A��A��A��A�{A1'A��A1�A@A��A�A�aAp;A �@�+@��b@�2�@���@��@�=@���@�Q@��@�dZ@��@�҉@�Ɇ@���@���@�� @���@���@���@�_p@��@�͟@�n�@��@�z@��@�(�@���@�5?@�T�@�w�@�_p@��5@���@��@� @�Q�@�p�@�O@��@���@�h
@��]@��#@�e�@�ی@�\�@�ݘ@�?�@�c�@�L@�oi@���@�F@�\�@��@ݵt@�	l@�)�@�qv@��@�H@�t�@�<6@��B@��3@�hs@��@֌�@՛=@ԭ�@�<�@�_@�~�@�-w@�S&@ҷ�@���@ь~@�K�@�=�@�(�@д9@�8�@φ�@��@ά@�@�@;w@Ͳ-@͎"@�:�@�C@��@ʌ�@�3�@���@�!�@�E�@Ǽ�@�o�@�4@��@Ƈ�@�j@�#:@Ř�@�Vm@ħ@�@�@�=q@�=q@�7�@èX@�@u@�Q�@�  @���@���@�8@��j@�~@���@���@���@��@�]d@�B[@�"h@��@��>@�p�@��@���@���@���@�q�@��.@�iD@�@@�z@�1�@��^@�x@�J#@��@�m�@��@�S�@��z@�YK@�@��S@�1�@��z@�4n@�� @��:@�o�@�@��O@�z�@�S�@�?�@�-�@��@��g@�S&@��@�w�@�@���@�b@��@��@���@�g8@���@�K�@�S@��K@�ی@�u%@�PH@��@��q@��"@� i@���@�xl@�4@��H@��@�IR@�͟@�4n@���@���@�\)@�V@��<@��@���@��k@�Q�@�ߤ@�ȴ@���@�u%@�>B@��@��@��6@�]�@�q@�V@���@�~(@�p;@�PH@��@�{J@�;@��X@��@��b@���@�7�@�{@��>@���@��-@���@�y�@�\)@�=�@��@�b@��@�c@�4�@�	l@�� @�:�@�	@��Z@���@���@�S&@�(�@�V@���@��Q@�k�@�)_@��@��I@�p;@�&�@���@��@�S�@��@���@�{�@�^5@�#:@���@�ϫ@���@�G�@�@���@���@�4n@���@���@�p�@�,�@�҉@�l�@�K^@�3�@��@�IR@��v@���@�d�@�L0@�)�@��@���@���@�1�@��}@�>B@��9@��@�l�@��@��L@���@�|�@�a|@�D�@���@��3@���@�s�@�(�@�@��)@���@�E�@��+@��-@��@�X�@�&@���@���@�:*@��m@��z@���@�_p@�6z@�;@���@���@��y@��@�{�@��@��W@��#@��t@���@�p�@���@��1@���@���@�tT@�G@��@��t@�|@�5�@�=�@�*0@�@��@���@�A�@��@�}�@� \@���@���@��D@�v�@�C-@��@C�@~��@~=q@}J�@|�9@|�.@|�D@|7@{�@zl�@z6�@y��@x�_@wƨ@w\)@vȴ@v8�@u�@u�d@u(�@t/�@s��@s4�@r�8@r�@rq�@r:*@q��@q��@q��@q+�@p�@p��@pm�@p  @o��@oP�@o+@n�8@n��@n$�@m�~@l�@l�u@l`�@l�@k>�@jV@j$�@i�@i�@i�@is�@i#�@hɆ@hq@h9X@h�@g�A@gC�@fȴ@f��@f��@fxl@fH�@f@�@f	@e�'@eO�@e&�@e	l@d�@d�p@dq@c�g@ce�@c,�@b��@bYK@b	@a�C@a�"@a+�@`e�@`G@_��@_"�@^�x@^V@^$�@]��@]��@]N<@]0�@]%F@\�f@\�@\:�@[�+@[�@[�@[��@[�f@[>�@[(@Z��@Z��@Z�1@Z@�@Y�Z@Y��@Y@X�o@X(�@W\)@W9�@W/�@W�@V��@V��@V+k@U��@Us�@U&�@T��@S��@S��@Sl�@S;d@R�+@Ru@Q��@Qj@P�@P"h@O�]@O�}@O��@O��@OdZ@O�@N�@N	@M�j@MrG@MA @L��@L$@K��@Kg�@KZ�@J�@J�r@I�o@IS&@H��@H�?@H��@Hu�@HXy@H1@G��@GU�@G!-@F�@F�]@Fc @E��@EG�@E�@D�5@D��@Dy>@D:�@Cݘ@C��@C��@Ct�@C\)@C8@B��@B-@Au�@A+@@�p@@�z@?�@?O@>�m@>-@=�M@=7L@=@@<�@<_@;�+@;�6@;H�@:ߤ@:��@:��@:~�@:v�@:\�@:O@9�C@9?}@8֡@8~(@8�@7��@7b�@7"�@6ߤ@6�@6kQ@6-@5�D@5�@5�>@5�@5�^@5L�@4�$@4_@4~@3�@3�P@3/�@3C@2��@2#:@1�d@1��@1=�@0�$@0z�@0M@0M@/�m@/��@/��@/��@/O@.��@.҉@.��@.1�@.�@-��@-F@,��@,q@,e�@,V�@,@+x@*��@*��@*��@*{�@*��@*p;@*:*@)�Z@)�S@)G�@(�v@(q@(S�@(-�@'��@'�A@'�g@'��@'�$@'g�@'@O@&��@&��@&��@&z@&.�@%��@%��@%hs@%�@$�$@$�@$oi@$V�@$7�@#x@"��@"p;@"�@!�H@!�'@!u�@!8�@ �`@ ~(@ /�@˒@�@&@�s@�<@�1@n�@GE@��@�@��@�'@rG@%F@��@z�@h�@C-@�W@��@�f@W?@(@�m@��@p;@3�@��@�#@��@|@B�@*0@�@�@V@��@�E@�@~(@bN@,=@�@��@�@=@
=@��@�X@�!@��@h
@�@IR@ \@��@��@�4@��@z�@V�@��@S�@�"@�B@��@��@��@~�@�@�n@��@w2@o @hs@X@IR@:�@!�@�@@�@�@Ɇ@��@��@r�@c�@?�@"h@�@�
@��@qv@;d@'�@�@�@��@��@��@�6@u%@W�@C�@($@�@
�@��@@�n@��@�@��@e,11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�eA��A��A��A�!�A�!�A��A��A�~A�.A�$A�-A�'�A�+6A�-�A�&�A�+�A�+kA�,�A�.}A�,�A�*eA�($A�$tA�+6A�.A�1A��yA�k�A��zA�T�A�?�A�6FA�.�A� 'A��A��A��A��A��8AϡbA�^�A��AȼAĺ�A�SA�o5A���A��<A�n�A�QA�k�A��7A�M�A��A�H�A�c�A�fA�'RA�҉A��nA�v�A�ܒA��^A���A��oA�@A�\A�xA��IA�~A�k�A���A��A�]dA��]A�B�A���A���A���A�ܒA�7�A�j�A���A�K^A|�#A{��Ax��At֡Ap�qAj�^AfߤAe��Ad�AbVmA`��A^��AZ��AX�AX(AW��AU=�AS�YAQ�XANOAK��AJ��AJ�<AHp;AE��ADz�AC��AB��AB;dA@�zA?S�A>OA<�'A;A9xlA7��A6��A4�A2_A0�"A-A(�MA&U2A%��A%{�A&$�A&0UA&A%HA%VA$ƨA$�+A$c A$MA#��A#D�A"h�A  iA�8AƨA��Au�A4A�WA��A~�AC�Av�A��A�:AA�.AhsA�AkQATaA�fA4AW?A>�A$tA)_A�bA��A��A��Ay>A+AE�A�A�A��A�XAv�A.�A��A�AI�A��A�A��A�fA��AxAb�A{A
QA	خA	1�A�FA�_A�1A��A��As�AH�A�A��A\�A6A�
A6zA�A�A�tA��A��A��A��A�{A1'A��A1�A@A��A�A�aAp;A �@�+@��b@�2�@���@��@�=@���@�Q@��@�dZ@��@�҉@�Ɇ@���@���@�� @���@���@���@�_p@��@�͟@�n�@��@�z@��@�(�@���@�5?@�T�@�w�@�_p@��5@���@��@� @�Q�@�p�@�O@��@���@�h
@��]@��#@�e�@�ی@�\�@�ݘ@�?�@�c�@�L@�oi@���@�F@�\�@��@ݵt@�	l@�)�@�qv@��@�H@�t�@�<6@��B@��3@�hs@��@֌�@՛=@ԭ�@�<�@�_@�~�@�-w@�S&@ҷ�@���@ь~@�K�@�=�@�(�@д9@�8�@φ�@��@ά@�@�@;w@Ͳ-@͎"@�:�@�C@��@ʌ�@�3�@���@�!�@�E�@Ǽ�@�o�@�4@��@Ƈ�@�j@�#:@Ř�@�Vm@ħ@�@�@�=q@�=q@�7�@èX@�@u@�Q�@�  @���@���@�8@��j@�~@���@���@���@��@�]d@�B[@�"h@��@��>@�p�@��@���@���@���@�q�@��.@�iD@�@@�z@�1�@��^@�x@�J#@��@�m�@��@�S�@��z@�YK@�@��S@�1�@��z@�4n@�� @��:@�o�@�@��O@�z�@�S�@�?�@�-�@��@��g@�S&@��@�w�@�@���@�b@��@��@���@�g8@���@�K�@�S@��K@�ی@�u%@�PH@��@��q@��"@� i@���@�xl@�4@��H@��@�IR@�͟@�4n@���@���@�\)@�V@��<@��@���@��k@�Q�@�ߤ@�ȴ@���@�u%@�>B@��@��@��6@�]�@�q@�V@���@�~(@�p;@�PH@��@�{J@�;@��X@��@��b@���@�7�@�{@��>@���@��-@���@�y�@�\)@�=�@��@�b@��@�c@�4�@�	l@�� @�:�@�	@��Z@���@���@�S&@�(�@�V@���@��Q@�k�@�)_@��@��I@�p;@�&�@���@��@�S�@��@���@�{�@�^5@�#:@���@�ϫ@���@�G�@�@���@���@�4n@���@���@�p�@�,�@�҉@�l�@�K^@�3�@��@�IR@��v@���@�d�@�L0@�)�@��@���@���@�1�@��}@�>B@��9@��@�l�@��@��L@���@�|�@�a|@�D�@���@��3@���@�s�@�(�@�@��)@���@�E�@��+@��-@��@�X�@�&@���@���@�:*@��m@��z@���@�_p@�6z@�;@���@���@��y@��@�{�@��@��W@��#@��t@���@�p�@���@��1@���@���@�tT@�G@��@��t@�|@�5�@�=�@�*0@�@��@���@�A�@��@�}�@� \@���@���@��D@�v�@�C-@��@C�@~��@~=q@}J�@|�9@|�.@|�D@|7@{�@zl�@z6�@y��@x�_@wƨ@w\)@vȴ@v8�@u�@u�d@u(�@t/�@s��@s4�@r�8@r�@rq�@r:*@q��@q��@q��@q+�@p�@p��@pm�@p  @o��@oP�@o+@n�8@n��@n$�@m�~@l�@l�u@l`�@l�@k>�@jV@j$�@i�@i�@i�@is�@i#�@hɆ@hq@h9X@h�@g�A@gC�@fȴ@f��@f��@fxl@fH�@f@�@f	@e�'@eO�@e&�@e	l@d�@d�p@dq@c�g@ce�@c,�@b��@bYK@b	@a�C@a�"@a+�@`e�@`G@_��@_"�@^�x@^V@^$�@]��@]��@]N<@]0�@]%F@\�f@\�@\:�@[�+@[�@[�@[��@[�f@[>�@[(@Z��@Z��@Z�1@Z@�@Y�Z@Y��@Y@X�o@X(�@W\)@W9�@W/�@W�@V��@V��@V+k@U��@Us�@U&�@T��@S��@S��@Sl�@S;d@R�+@Ru@Q��@Qj@P�@P"h@O�]@O�}@O��@O��@OdZ@O�@N�@N	@M�j@MrG@MA @L��@L$@K��@Kg�@KZ�@J�@J�r@I�o@IS&@H��@H�?@H��@Hu�@HXy@H1@G��@GU�@G!-@F�@F�]@Fc @E��@EG�@E�@D�5@D��@Dy>@D:�@Cݘ@C��@C��@Ct�@C\)@C8@B��@B-@Au�@A+@@�p@@�z@?�@?O@>�m@>-@=�M@=7L@=@@<�@<_@;�+@;�6@;H�@:ߤ@:��@:��@:~�@:v�@:\�@:O@9�C@9?}@8֡@8~(@8�@7��@7b�@7"�@6ߤ@6�@6kQ@6-@5�D@5�@5�>@5�@5�^@5L�@4�$@4_@4~@3�@3�P@3/�@3C@2��@2#:@1�d@1��@1=�@0�$@0z�@0M@0M@/�m@/��@/��@/��@/O@.��@.҉@.��@.1�@.�@-��@-F@,��@,q@,e�@,V�@,@+x@*��@*��@*��@*{�@*��@*p;@*:*@)�Z@)�S@)G�@(�v@(q@(S�@(-�@'��@'�A@'�g@'��@'�$@'g�@'@O@&��@&��@&��@&z@&.�@%��@%��@%hs@%�@$�$@$�@$oi@$V�@$7�@#x@"��@"p;@"�@!�H@!�'@!u�@!8�@ �`@ ~(@ /�@˒@�@&@�s@�<@�1@n�@GE@��@�@��@�'@rG@%F@��@z�@h�@C-@�W@��@�f@W?@(@�m@��@p;@3�@��@�#@��@|@B�@*0@�@�@V@��@�E@�@~(@bN@,=@�@��@�@=@
=@��@�X@�!@��@h
@�@IR@ \@��@��@�4@��@z�@V�@��@S�@�"@�B@��@��@��@~�@�@�n@��@w2@o @hs@X@IR@:�@!�@�@@�@�@Ɇ@��@��@r�@c�@?�@"h@�@�
@��@qv@;d@'�@�@�@��@��@��@�6@u%@W�@C�@($@�@
�@��@@�n@��@�@��@e,11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	}�B	}�B	}�B	}�B	}VB	}�B	~B	~BB	~wB	~]B	}�B	}�B	}�B	}�B	}�B	~BB	~�B	~�B	B	~�B	B	}B	�B	�B	cB	HB	�iB	�B	��B	�<B	�B	��B	��B	�4B	�B	�4B	��B	�0B	{�B	_;B	P�B	7�B	,"B	# B	+B	^B	wB	�7B	��B	��B	��B	οB	ɺB	�B
PB
[#B
��B
�pBB�BaB�B�B
��B
�9B
�9B
��B
��B
�"BB6�BaHBgBp;BtB�9B��B�#Bs�BgB
�B
4�B	�B	��B	��B	w2B	vB	qvB	m�B	Z�B	JrB	G_B	G�B	G_B	H�B	IlB	J�B	P.B	PHB	PbB	O�B	ZB	ZkB	TaB	M�B	I�B	F�B	CB	<PB	/�B	)�B	/�B	5�B	@�B	@�B	IRB	Q B	[WB	`�B	dZB	g�B	n�B	]IB	I�B	<�B	�B�B�B�IB	MB	$�B	+B	@OB	[�B	`�B	e�B	e`B	e`B	e�B	e�B	f2B	h$B	vB	y�B	{JB	{�B	|PB	|�B	|B	{�B	zDB	x�B	v�B	r-B	o�B	oB	q�B	v�B	vFB	t�B	vzB	v�B	u%B	y>B	��B	�3B	��B	�(B	�[B	�EB	�B	��B	�?B	�B	�OB	��B	�[B	��B	��B	��B	��B	��B	�3B	�SB	�B	��B	ǔB	�1B	ɠB	��B	��B	�JB	��B	�B	�4B	�_B	�]B	�OB	߾B	��B	�B	�4B	�:B	�TB	� B	�nB	�B	�nB	��B	�B	�B	�B	��B	�\B	��B	�BB	�B	�-B	��B	��B	��B	�B	��B	�4B	��B	��B	�B	�B	�B	�B	��B	�4B	�B	�NB	��B	�B	�B	�hB	�B	�HB	�:B	�-B	��B	��B	��B	�\B	�'B	��B	�pB	�;B	��B	�pB	�pB	�VB	ߤB	��B	ߊB	�vB	��B	��B	�B	ߤB	��B	��B	޸B	�B	�!B	��B	�B	�;B	��B	�OB	یB	یB	�QB	��B	��B	��B	��B	��B	�B	��B	��B	�;B	�pB	ޞB	ބB	�5B	��B	ޞB	ޞB	�!B	�B	�BB	ޞB	�B	�OB	�pB	��B	��B	�B	�B	� B	�B	�B	��B	��B	�B	߾B	�;B	�B	��B	��B	�B	�B	� B	��B	�B	��B	��B	��B	�:B	�ZB	��B	�ZB	�B	�`B	�B	�B	�8B	��B	�B	��B	�B	�B	�_B	�B	�B	�B	�B	�wB	�IB	�/B	�B	�IB	�}B	��B	�IB	�B	�B	�B	�B	�UB	�B	��B	�B	��B	��B	��B	��B	�FB	�+B	��B	�zB	�+B	�B	�TB	�TB	�nB	��B	��B	��B	�B	�?B	��B	��B	��B	�FB	�FB	��B	�`B	�2B	��B	�B	��B	�B	��B	�^B	�^B	��B	�JB	��B	�B	��B	��B	��B	��B
[B
�B
'B
 �B	�}B	��B	��B	�(B	�(B	�(B	��B	��B	�cB
 B
�B
'B
-B
3B
mB
�B
tB
�B
�B
�B
�B
B
�B
�B
zB
zB
�B
�B
�B
fB
fB
�B
�B
�B
	lB
	�B
	�B
JB
�B
�B
�B
�B
�B
�B
�B
"B
�B
�B
<B
VB
�B
�B
�B
B
\B
\B
�B
bB
}B
�B
�B
�B
 B
 B
�B
[B
�B
uB
uB
�B
uB
�B
�B
�B
:B
oB
�B
 B
�B
uB
�B
[B
uB
uB
uB
�B
�B
�B
B
2B
B
B
B
MB
gB
�B
�B
�B
mB
?B
$B
sB
�B
_B
�B
B
�B
�B
B
�B
KB
eB
eB
�B
�B
7B
�B
�B
�B
�B
�B
�B
B
B
!B
B
�B
B
VB
�B
�B
�B
�B
�B
�B
�B
 'B
 BB
 �B
 �B
 �B
!HB
!bB
!�B
!�B
"�B
# B
#:B
#nB
#�B
$@B
$�B
%FB
&LB
&�B
'B
'RB
)_B
*KB
+B
+kB
+�B
+�B
+�B
,=B
-B
-�B
.B
-�B
-�B
.IB
.�B
.�B
/iB
/iB
/5B
/B
/B
0�B
1AB
1�B
2-B
2-B
2aB
2�B
2�B
2�B
2�B
3hB
3�B
3�B
4nB
4�B
4�B
4�B
5%B
5�B
6B
5�B
6+B
7B
7�B
8B
8B
8lB
8�B
8�B
9rB
9XB
9�B
9�B
9�B
:B
:DB
:DB
:^B
:xB
:xB
:�B
:�B
:�B
;B
;0B
;B
;�B
;�B
<B
<6B
<PB
<�B
="B
=VB
=VB
=qB
>BB
>�B
?B
?.B
?B
?B
?�B
?�B
?�B
@4B
@iB
@OB
@iB
A;B
A;B
A;B
AUB
AoB
A�B
A�B
A�B
A�B
BB
B'B
B'B
BAB
B'B
B�B
C-B
CGB
CaB
C�B
DB
D�B
D�B
D�B
EB
E�B
F%B
E�B
F�B
GB
G+B
G_B
G_B
G�B
G�B
G�B
G�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IRB
IRB
IlB
IlB
IlB
I�B
IRB
H�B
IRB
I�B
I�B
J�B
K)B
K^B
K�B
K�B
K�B
LJB
LdB
L�B
LdB
MB
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P.B
P�B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
R:B
RTB
R�B
SB
S[B
S[B
SuB
SuB
S[B
S�B
S�B
TB
TFB
T{B
TaB
T�B
T�B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
W
B
W
B
W$B
WsB
W�B
XyB
X�B
X�B
X�B
YKB
Y�B
Y�B
Z7B
[	B
[#B
[	B
[	B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]~B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_;B
_pB
_�B
_�B
_�B
_�B
_pB
_�B
`BB
`vB
`�B
`�B
`�B
aB
`�B
abB
a�B
a�B
a�B
b4B
b�B
b�B
b�B
cB
c:B
c:B
c:B
cTB
c�B
c�B
c�B
c�B
d@B
d@B
d@B
d�B
d�B
eFB
e,B
eB
eFB
e�B
fLB
f2B
f2B
f2B
f2B
fLB
ffB
f�B
f�B
f�B
g8B
g�B
g�B
g�B
h
B
g�B
h$B
h$B
h>B
hXB
hsB
h�B
h�B
h�B
h�B
i*B
iDB
i_B
i�B
i�B
j0B
j0B
jeB
jKB
j0B
kB
k�B
k�B
lB
lWB
lWB
lqB
l�B
l�B
mCB
m]B
m�B
m�B
ncB
n�B
n�B
n�B
o B
o B
oiB
o�B
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
r-B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
t9B
tTB
tTB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
vB
v+B
vFB
vFB
v�B
wfB
w�B
w�B
w�B
xB
xB
xB
xB
xlB
y	B
y>B
yrB
y�B
y�B
yXB
yrB
zB
z*B
zDB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{B
{0B
{0B
{0B
{JB
{dB
{JB
{�B
{�B
{�B
{�B
|6B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	}�B	}�B	}�B	}�B	}VB	}�B	~B	~BB	~wB	~]B	}�B	}�B	}�B	}�B	}�B	~BB	~�B	~�B	B	~�B	B	}B	�B	�B	cB	HB	�iB	�B	��B	�<B	�B	��B	��B	�4B	�B	�4B	��B	�0B	{�B	_;B	P�B	7�B	,"B	# B	+B	^B	wB	�7B	��B	��B	��B	οB	ɺB	�B
PB
[#B
��B
�pBB�BaB�B�B
��B
�9B
�9B
��B
��B
�"BB6�BaHBgBp;BtB�9B��B�#Bs�BgB
�B
4�B	�B	��B	��B	w2B	vB	qvB	m�B	Z�B	JrB	G_B	G�B	G_B	H�B	IlB	J�B	P.B	PHB	PbB	O�B	ZB	ZkB	TaB	M�B	I�B	F�B	CB	<PB	/�B	)�B	/�B	5�B	@�B	@�B	IRB	Q B	[WB	`�B	dZB	g�B	n�B	]IB	I�B	<�B	�B�B�B�IB	MB	$�B	+B	@OB	[�B	`�B	e�B	e`B	e`B	e�B	e�B	f2B	h$B	vB	y�B	{JB	{�B	|PB	|�B	|B	{�B	zDB	x�B	v�B	r-B	o�B	oB	q�B	v�B	vFB	t�B	vzB	v�B	u%B	y>B	��B	�3B	��B	�(B	�[B	�EB	�B	��B	�?B	�B	�OB	��B	�[B	��B	��B	��B	��B	��B	�3B	�SB	�B	��B	ǔB	�1B	ɠB	��B	��B	�JB	��B	�B	�4B	�_B	�]B	�OB	߾B	��B	�B	�4B	�:B	�TB	� B	�nB	�B	�nB	��B	�B	�B	�B	��B	�\B	��B	�BB	�B	�-B	��B	��B	��B	�B	��B	�4B	��B	��B	�B	�B	�B	�B	��B	�4B	�B	�NB	��B	�B	�B	�hB	�B	�HB	�:B	�-B	��B	��B	��B	�\B	�'B	��B	�pB	�;B	��B	�pB	�pB	�VB	ߤB	��B	ߊB	�vB	��B	��B	�B	ߤB	��B	��B	޸B	�B	�!B	��B	�B	�;B	��B	�OB	یB	یB	�QB	��B	��B	��B	��B	��B	�B	��B	��B	�;B	�pB	ޞB	ބB	�5B	��B	ޞB	ޞB	�!B	�B	�BB	ޞB	�B	�OB	�pB	��B	��B	�B	�B	� B	�B	�B	��B	��B	�B	߾B	�;B	�B	��B	��B	�B	�B	� B	��B	�B	��B	��B	��B	�:B	�ZB	��B	�ZB	�B	�`B	�B	�B	�8B	��B	�B	��B	�B	�B	�_B	�B	�B	�B	�B	�wB	�IB	�/B	�B	�IB	�}B	��B	�IB	�B	�B	�B	�B	�UB	�B	��B	�B	��B	��B	��B	��B	�FB	�+B	��B	�zB	�+B	�B	�TB	�TB	�nB	��B	��B	��B	�B	�?B	��B	��B	��B	�FB	�FB	��B	�`B	�2B	��B	�B	��B	�B	��B	�^B	�^B	��B	�JB	��B	�B	��B	��B	��B	��B
[B
�B
'B
 �B	�}B	��B	��B	�(B	�(B	�(B	��B	��B	�cB
 B
�B
'B
-B
3B
mB
�B
tB
�B
�B
�B
�B
B
�B
�B
zB
zB
�B
�B
�B
fB
fB
�B
�B
�B
	lB
	�B
	�B
JB
�B
�B
�B
�B
�B
�B
�B
"B
�B
�B
<B
VB
�B
�B
�B
B
\B
\B
�B
bB
}B
�B
�B
�B
 B
 B
�B
[B
�B
uB
uB
�B
uB
�B
�B
�B
:B
oB
�B
 B
�B
uB
�B
[B
uB
uB
uB
�B
�B
�B
B
2B
B
B
B
MB
gB
�B
�B
�B
mB
?B
$B
sB
�B
_B
�B
B
�B
�B
B
�B
KB
eB
eB
�B
�B
7B
�B
�B
�B
�B
�B
�B
B
B
!B
B
�B
B
VB
�B
�B
�B
�B
�B
�B
�B
 'B
 BB
 �B
 �B
 �B
!HB
!bB
!�B
!�B
"�B
# B
#:B
#nB
#�B
$@B
$�B
%FB
&LB
&�B
'B
'RB
)_B
*KB
+B
+kB
+�B
+�B
+�B
,=B
-B
-�B
.B
-�B
-�B
.IB
.�B
.�B
/iB
/iB
/5B
/B
/B
0�B
1AB
1�B
2-B
2-B
2aB
2�B
2�B
2�B
2�B
3hB
3�B
3�B
4nB
4�B
4�B
4�B
5%B
5�B
6B
5�B
6+B
7B
7�B
8B
8B
8lB
8�B
8�B
9rB
9XB
9�B
9�B
9�B
:B
:DB
:DB
:^B
:xB
:xB
:�B
:�B
:�B
;B
;0B
;B
;�B
;�B
<B
<6B
<PB
<�B
="B
=VB
=VB
=qB
>BB
>�B
?B
?.B
?B
?B
?�B
?�B
?�B
@4B
@iB
@OB
@iB
A;B
A;B
A;B
AUB
AoB
A�B
A�B
A�B
A�B
BB
B'B
B'B
BAB
B'B
B�B
C-B
CGB
CaB
C�B
DB
D�B
D�B
D�B
EB
E�B
F%B
E�B
F�B
GB
G+B
G_B
G_B
G�B
G�B
G�B
G�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IRB
IRB
IlB
IlB
IlB
I�B
IRB
H�B
IRB
I�B
I�B
J�B
K)B
K^B
K�B
K�B
K�B
LJB
LdB
L�B
LdB
MB
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P.B
P�B
P�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
R:B
RTB
R�B
SB
S[B
S[B
SuB
SuB
S[B
S�B
S�B
TB
TFB
T{B
TaB
T�B
T�B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
W
B
W
B
W$B
WsB
W�B
XyB
X�B
X�B
X�B
YKB
Y�B
Y�B
Z7B
[	B
[#B
[	B
[	B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]~B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_;B
_pB
_�B
_�B
_�B
_�B
_pB
_�B
`BB
`vB
`�B
`�B
`�B
aB
`�B
abB
a�B
a�B
a�B
b4B
b�B
b�B
b�B
cB
c:B
c:B
c:B
cTB
c�B
c�B
c�B
c�B
d@B
d@B
d@B
d�B
d�B
eFB
e,B
eB
eFB
e�B
fLB
f2B
f2B
f2B
f2B
fLB
ffB
f�B
f�B
f�B
g8B
g�B
g�B
g�B
h
B
g�B
h$B
h$B
h>B
hXB
hsB
h�B
h�B
h�B
h�B
i*B
iDB
i_B
i�B
i�B
j0B
j0B
jeB
jKB
j0B
kB
k�B
k�B
lB
lWB
lWB
lqB
l�B
l�B
mCB
m]B
m�B
m�B
ncB
n�B
n�B
n�B
o B
o B
oiB
o�B
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
r-B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
t9B
tTB
tTB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
vB
v+B
vFB
vFB
v�B
wfB
w�B
w�B
w�B
xB
xB
xB
xB
xlB
y	B
y>B
yrB
y�B
y�B
yXB
yrB
zB
z*B
zDB
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{B
{0B
{0B
{0B
{JB
{dB
{JB
{�B
{�B
{�B
{�B
|6B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
}B
}"B
}<B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191958  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191958  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191958                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042007  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042007  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                