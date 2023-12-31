CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-12-01T18:05:24Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       C|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20171201180524  20181025093511  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�9�*z*�1   @�9�UUav@:}p��
=�cE����1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��R@��RA\)A?\)A`��A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`wD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��+A��+A��7A��7A��DA��\A��hA��hA��uA���A���A���A���A��PA�M�A��A���A��
A�A��A��+A�G�A��A��HA���A��9A�C�A���A��A�ZA�l�A��PA���A� �A�ȴA�+A��7A��uA���A�JA�jA��A�n�A�A�A���A��A�I�A�ȴA���A���A��
A�~�A�XA�G�A��A���A�VA���A���A�^5A���A��A�(�A�VA��!A��yA���A�VA�%A�$�A��hA�9XA��;A��DA�M�A�|�A��uA��7A�1'A�ĜA���A�A�E�A�bA�ƨA��#A��A�bA�jA�$�A��!A�jA��
A�?}A~�HA}��A}�A{XAy�PAxAw��Aw
=Av�Au�At��Ar��Ao�TAn�`Am�Am�hAl�Ai�Ai�#AiK�Af�\A_��A\ĜAYVAV�/ATffAR�yAO�TAM;dALbNAL�!AL��AL=qAL9XAK�hAJ�AI��AGƨAF�RAF9XAEx�ADVA@�RA?��A?\)A?dZA?33A>��A=;dA;�7A9A8ffA7�-A7+A6=qA5�A4A2��A2�uA2A1oA05?A/A/\)A.�HA.ffA-�A,�A,�A*�/A*ZA*=qA)�A)��A(��A'�TA'�FA'l�A'�A%S�A$�+A#�A#l�A"��A"z�A"9XA!�7A!VA n�A�yA��AQ�A�7A�RAO�AVA��A�Ar�AA��A�-A\)A�Ar�A�
A
=AA�AjA��AK�A��A�FA��AVA1A��A��A�TA
��A
=qA	G�AZAx�AC�A&�AQ�AhsA"�A�9An�AM�A��A�7A�A�DAVA-A��@��@�E�@���@���@�;d@��@��@��@��@�|�@��H@�V@���@���@�K�@���@�O�@�@��m@�33@�M�@�1'@�33@�!@⟾@�5?@��@���@�;d@ޗ�@��@�{@���@�/@���@Չ7@�A�@ӍP@��@�ff@��`@ϝ�@Ώ\@͉7@�bN@�\)@�^5@Ɂ@�Z@��@�ff@Ł@ě�@�j@�b@�S�@�=q@���@�t�@�n�@���@��F@�C�@���@���@�t�@�V@��@�ƨ@�33@��!@�v�@�M�@���@�9X@��P@���@��@��@���@�v�@�M�@���@�hs@��j@�1@��@��+@��^@�hs@�Ĝ@�9X@�ƨ@�dZ@��R@�$�@��h@�p�@�/@�A�@��;@�33@���@���@�$�@��@�x�@��@��@�9X@��@�K�@�v�@��T@��h@�&�@��9@��
@��@��!@�ff@���@���@�G�@���@�9X@�;d@�~�@�=q@�J@��^@�x�@���@�z�@�Z@�b@��;@���@���@�K�@���@�M�@�$�@��#@��@�X@�?}@�/@�7L@��@���@��9@��D@���@�33@�o@��@�v�@�^5@�V@�=q@�$�@��#@�X@���@�9X@��;@���@��@�o@�
=@��y@��R@�ȴ@��@�33@�\)@�"�@��H@��R@��+@�n�@�^5@�ff@�^5@�=q@�-@�J@���@�z�@�|�@��R@���@���@�x�@��@��u@�A�@�@�;@�@�1'@�;@��@K�@~��@~5?@}@|�@|j@|I�@{��@{o@{33@{@{"�@{o@{��@y��@xb@w�;@w�;@w��@w\)@w�@v��@v��@vV@u��@u�h@u`B@t�j@t�@tz�@t9X@s��@st�@r�H@rn�@rM�@r�@r�@r�@qhs@qG�@qx�@q�7@q7L@pĜ@pĜ@pbN@pA�@pbN@pb@o��@o|�@ol�@n�y@n�@nȴ@nV@m�-@m�@mO�@l��@l�D@lj@l�@kƨ@kdZ@kC�@j�@j�!@j��@j�\@jn�@jM�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��+A��+A��7A��7A��DA��\A��hA��hA��uA���A���A���A���A��PA�M�A��A���A��
A�A��A��+A�G�A��A��HA���A��9A�C�A���A��A�ZA�l�A��PA���A� �A�ȴA�+A��7A��uA���A�JA�jA��A�n�A�A�A���A��A�I�A�ȴA���A���A��
A�~�A�XA�G�A��A���A�VA���A���A�^5A���A��A�(�A�VA��!A��yA���A�VA�%A�$�A��hA�9XA��;A��DA�M�A�|�A��uA��7A�1'A�ĜA���A�A�E�A�bA�ƨA��#A��A�bA�jA�$�A��!A�jA��
A�?}A~�HA}��A}�A{XAy�PAxAw��Aw
=Av�Au�At��Ar��Ao�TAn�`Am�Am�hAl�Ai�Ai�#AiK�Af�\A_��A\ĜAYVAV�/ATffAR�yAO�TAM;dALbNAL�!AL��AL=qAL9XAK�hAJ�AI��AGƨAF�RAF9XAEx�ADVA@�RA?��A?\)A?dZA?33A>��A=;dA;�7A9A8ffA7�-A7+A6=qA5�A4A2��A2�uA2A1oA05?A/A/\)A.�HA.ffA-�A,�A,�A*�/A*ZA*=qA)�A)��A(��A'�TA'�FA'l�A'�A%S�A$�+A#�A#l�A"��A"z�A"9XA!�7A!VA n�A�yA��AQ�A�7A�RAO�AVA��A�Ar�AA��A�-A\)A�Ar�A�
A
=AA�AjA��AK�A��A�FA��AVA1A��A��A�TA
��A
=qA	G�AZAx�AC�A&�AQ�AhsA"�A�9An�AM�A��A�7A�A�DAVA-A��@��@�E�@���@���@�;d@��@��@��@��@�|�@��H@�V@���@���@�K�@���@�O�@�@��m@�33@�M�@�1'@�33@�!@⟾@�5?@��@���@�;d@ޗ�@��@�{@���@�/@���@Չ7@�A�@ӍP@��@�ff@��`@ϝ�@Ώ\@͉7@�bN@�\)@�^5@Ɂ@�Z@��@�ff@Ł@ě�@�j@�b@�S�@�=q@���@�t�@�n�@���@��F@�C�@���@���@�t�@�V@��@�ƨ@�33@��!@�v�@�M�@���@�9X@��P@���@��@��@���@�v�@�M�@���@�hs@��j@�1@��@��+@��^@�hs@�Ĝ@�9X@�ƨ@�dZ@��R@�$�@��h@�p�@�/@�A�@��;@�33@���@���@�$�@��@�x�@��@��@�9X@��@�K�@�v�@��T@��h@�&�@��9@��
@��@��!@�ff@���@���@�G�@���@�9X@�;d@�~�@�=q@�J@��^@�x�@���@�z�@�Z@�b@��;@���@���@�K�@���@�M�@�$�@��#@��@�X@�?}@�/@�7L@��@���@��9@��D@���@�33@�o@��@�v�@�^5@�V@�=q@�$�@��#@�X@���@�9X@��;@���@��@�o@�
=@��y@��R@�ȴ@��@�33@�\)@�"�@��H@��R@��+@�n�@�^5@�ff@�^5@�=q@�-@�J@���@�z�@�|�@��R@���@���@�x�@��@��u@�A�@�@�;@�@�1'@�;@��@K�@~��@~5?@}@|�@|j@|I�@{��@{o@{33@{@{"�@{o@{��@y��@xb@w�;@w�;@w��@w\)@w�@v��@v��@vV@u��@u�h@u`B@t�j@t�@tz�@t9X@s��@st�@r�H@rn�@rM�@r�@r�@r�@qhs@qG�@qx�@q�7@q7L@pĜ@pĜ@pbN@pA�@pbN@pb@o��@o|�@ol�@n�y@n�@nȴ@nV@m�-@m�@mO�@l��@l�D@lj@l�@kƨ@kdZ@kC�@j�@j�!@j��@j�\@jn�@jM�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�?B�9B�3B�3B�3B�3B�LB�jB�}BÖBȴBȴB��B��B��B�#B�)B�;B�BBBB  B��B��B��B��B��B��B�B��B��B��B�sB�/B��B�wB�RB�B��B��B��B�oB�BiyBVBO�BL�BP�BN�BL�B>wB.B�B\BPBVB\BPB	7B��B�B�sB�mB�sB�/BȴB��B�oB�7B�B� Bp�BgmB`BB]/BZBVBP�BA�B6FB'�B!�B�BVBB
��B
��B
�mB
ŢB
�?B
��B
��B
��B
��B
��B
�{B
�PB
�+B
}�B
w�B
jB
aHB
XB
T�B
P�B
L�B
G�B
>wB
.B
{B
DB
B	��B	�B	�;B	�/B	��B	�-B	l�B	A�B	oB	B�yB�
B�qB�B�?BɺB��B��B�;B�fB��B��B�B�B�yB�TB�B�dB�'B�3B�?B�qB��B�LB�B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�\B�PB�JB�DB�7B�+B�1B�1B�1B�1B�%B�B�B�B~�B{�B{�B|�B|�B|�B{�Bz�Bz�By�Bv�Bq�Bk�Be`BdZB_;BVBS�BS�BS�BS�BT�BYBaHBcTBbNBbNBcTBaHB_;B]/B[#BZBW
BR�BO�BM�BL�BL�BJ�BH�BE�BC�BA�B?}B<jB;dB:^B8RB6FB6FB6FB6FB6FB9XB<jBB�BC�BC�BC�BB�B?}B=qB=qB=qB=qB;dB6FB0!B/B.B-B+B)�B(�B(�B+B+B+B+B+B)�B)�B'�B(�B(�B(�B(�B'�B%�B#�B#�B$�B$�B �B�B{B�B�B�BuBbB\B\BhBuBuB{B{B�B�B�B�B�B�B�B�B�B�B �B"�B$�B%�B%�B$�B!�B#�B$�B$�B$�B&�B(�B/B33B33B33B49B7LB8RB9XB;dB?}BA�BD�BE�BF�BJ�BM�BP�BR�BS�BT�BW
BYBYBYBZB\)B_;B`BB`BB`BBcTBjBm�Bp�Bu�Bv�Bw�B� B�B�B�B�B�7B�DB�VB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�9B�?B�LB�RB�XB�jB�wB�}B��BĜBŢBȴBȴBɺB��B��B��B��B�
B�#B�5B�NB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	1B	
=B	VB	\B	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	(�B	)�B	+B	0!B	2-B	2-B	/B	/B	.B	.B	.B	/B	0!B	2-B	5?B	8RB	<jB	>wB	A�B	A�B	B�B	D�B	D�B	E�B	G�B	H�B	J�B	K�B	M�B	O�B	O�B	R�B	T�B	YB	\)B	^5B	_;B	`BB	aHB	bNB	bNB	cTB	e`B	hsB	jB	l�B	m�B	q�B	q�B	s�B	u�B	v�B	x�B	{�B	~�B	� B	�B	�B	�B	�B	�+B	�DB	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�RB	�XB	�XB	�XB	�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�5B�?B�4B�+B�4B�+B�OB�kB�pBÏBȸBȱBʳB˝B��B�UB܇B�HB�B�B�BdB cB��B�#B��B�&B�(B�LB��B��BB��B�cB�B��B�5B�[B�B��B�KB�aB��B�HBp�B^4BUBO�BS�BQ9BQ�BBxB4�B}B�B�B�BB?B�B��B�B�B�B�B��B��B�*B��B��B�XB��Bt�Bi�Ba�B^�B[�BWHBXJBE|B:�B)�B#�B�BbB�B )B�B
��B
��B
�CB
�B
�FB
�+B
�B
��B
��B
��B
�0B
�B
|�B
omB
e`B
YB
V�B
RsB
NpB
J�B
D�B
5�B
iB
�B
rB
gB	�}B	ߜB	�,B	�BB	�>B	v+B	K�B	�B	�B��BߘB�nB�SB�vB��B��B��B�!B�FB��B	�B�VB��B�B��B�kB��B��B�(B��B��BĵB��B��B��B��B�B�
B�uB�'B��B�5B�B�B��B��B��B��B��B��B�B��B��B��B��B�B�-B�EB�=B��B��B�9B�$B~FB}�B~fB~>B~nB|�B|�B|[B{�B{Bu�Bo'Bg�BgBcBV�BUBTeBT,BU BU�BX�BbBd6BczBc�Be.Bc'BcCB_B[�BZ�BY�BU�BP�BN�BM�BNgBMfBKsBG=BF!BC�BA�B<�B;�B<B:�B7B7yB7B6�B7=B:�B>KBCjBD-BD BEKBG�BA�B?=B?B>NB?:B>�B;kB4?B0�B.�B-�B,B+B+VB*�B+�B,B+�B+�B,JB,�B+PB(�B)B)�B*qB*�B(�B&�B$vB#�B$�B%�B%BB�BNB6B&B1B�B�B�B�B�B�B�BB*B�B�B�B�B#B�B'B�B�B"'B$�B&:B&�B&�B'}B#�B%UB&wB&�B%�B'�B)FB/VB4B4�B4B5SB8JB:�B:�B;�B?�BB*BE"BF�BG�BKjBO%BQ�BSgBT�BU�BW�BY�BZBY�BZ�B\`B_�Ba�B`�Ba:Bc�Bj�Bn=Bp�Bv�Bw�BxhB�|B��B��B�OB��B��B��B�	B��B��B�)B�	B�{B�~B��B��B�NB��B��B�/B�1B�ZB�VB��B��B�aB��B�~B��B�OB��B�7B��B��B��B�B��B��B��BȵB�B��B�;B�,B��B�B�ZB�qB��B�B�B��B��B�-B��B�sB��B�]B�JB�+B	 �B	1B	fB	
�B	CB	$B	B	]B	B	B	�B	�B	�B	!�B	#�B	&�B	)/B	*B	+FB	0�B	3�B	3�B	0EB	00B	.�B	.iB	.�B	/�B	0�B	2�B	5PB	8HB	<"B	>�B	A�B	A�B	C#B	D�B	D�B	F�B	G�B	H�B	KMB	L?B	M�B	PB	O�B	SB	T�B	Z�B	]aB	^_B	_DB	`{B	a�B	b�B	boB	c�B	e�B	h�B	j�B	l�B	nB	q�B	q�B	s�B	u�B	w+B	yAB	|8B	B	�&B	�B	� B	��B	�<B	�B	�@B	��B	��B	�|B	��B	��B	�|B	��B	��B	��B	��B	�5B	��B	��B	�;B	�XB	�B	�B	�=B	�bB	�3B	�VB	�fB	�xB	�YB	��B	�yB	�cB	�jB	�qB	�mB	�a1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*3<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�9"<.n�<7�:<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.04 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352142018102313521420181023135214  AO  ARCAADJP                                                                    20171201180524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171201180524  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171201180524  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135214  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20181023135214  QC  PSAL            @�33D�� G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093511  IP                  G�O�G�O�G�O�                