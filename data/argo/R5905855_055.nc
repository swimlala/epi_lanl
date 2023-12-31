CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:20:11Z creation;2022-06-04T19:20:11Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192011  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�/�����1   @�/�����@.�1&��cpbM��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�33Bș�B�33B�ffB���B�  B�  B�  B�  B�  B�  BB�  B�  B�  C   C  C  C  C  C
  C�C33C  C  C�fC  C  C  C�fC  C   C"  C$  C&  C(  C*�C+��C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CRL�CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�C3Dƃ3D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A��A��GA߮A�A��B�
B=pB�
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
B��B��B��B��B��B��B��B��B��B��B��RB��B��B��B��B��B��BȅB��B�Q�BӸRB��B��B��B��B��B��B�B��B��B��B��C��C��C��C��C	��C]C(�C��C��C�)C��C��C��C�)C��C��C!��C#��C%��C'��C*]C+C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CRB�CS�)CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD��D�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�A�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�{�Dľ�D���D�>�D�~�Dž�D���D�A�DƁ�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�e�A�e�A�f�A�iyA�kQA�m]A�n�A�n�A�p�A�qA�qA�qA�qAA�o�A�m�A�o�A�qvA�n�A�j�A�h�A�d�A�g8A�e�A�XyA�B[A�9�A�7�A��Aׯ�A�c�A��A��KA���A��A�{JA�1'AЍ�A˛qA�.A���A��AǡbAƼ�AĠ'A��?A�v�A�a�A�$A�'RA�*�A�U2A�)*A�O�A���A�ҽA�:�A���A�ƨA�:A���A��A�%zA���A���A�A��iA��#A�X�A�z�A��A�5tA���A�FA���A�o�A���A���A�XEA�r�A�J#A��A�+�A�7A��DA~�;A{��At�)Ao��AmcAm�Ak��Af�Ac��A_��A[�AW��AV�AR��AP�AMk�AIW?AC{A=��A:�0A8�A8?�A7OA5��A5�A4�A3�A3L0A2��A/^�A.�A+�'A(� A&�MA&6zA%zxA$��A% \A$�XA$e�A#��A"�;A"9�A!/�A �@A YKA�"A
�A�Ax�Aw2A;�AoA�^A�A�A��A�_A�$A�AJ�AFA�hA��A��A?�A-�A�A�A��A��Af�A��A�`A6�Ap;AeA�	A�A~Ao�A��AԕAi�Ah�AQ�A�A�dA��A�XA�IAHA�uA�A�A��A�MA,=A#�A�A��Ay�A2�A
��A
��A
�A	�A	��A	=�A�A�*Ay>AV�A�+A8�AϫAq�Ax�A0UA�vA��A��Al�AN�A%A�A�XA�:A�Am]A  A��AS�A�xAS&A�A�A��A4�A�6A��Al�A4�A�A �8A �/A �XA �KA �'A �@�a@��@���@�n�@�I�@���@��P@�.I@��6@�
�@�Y�@���@��)@�z�@�}V@��@�ԕ@��@�$@�J#@�"h@�x�@�f�@�e�@��@�A@�@��@�\@�1�@��;@�hs@�PH@��@�`�@힄@�5�@빌@�J#@��@�(�@�+@�خ@�o@��|@�R@思@���@��p@�1'@㔯@��,@�`�@�J@�y�@�.I@�0U@߄M@�{J@�a@�v�@ݓ@�`B@��@�h�@��@ښ�@ڇ�@ڤ�@ڳh@گO@�GE@�4@�ƨ@�T�@���@�H�@�'R@��@�G@��3@�@�0U@��@��6@�@�b�@ԇ�@ӝ�@�Y@Ҟ@�h
@�'R@ѓ@� \@���@�}V@�1@ϝ�@�o�@�S&@�@��5@ΰ!@�>B@�˒@�s@�!�@̎�@�8�@��@˼�@�j@�*0@��@���@���@��@�ȴ@ʾ@ʕ�@�xl@��@�y>@���@��3@�;@�$�@���@��@žw@�rG@�ں@Ĭ@�Ov@�J�@¯O@��@�@�?@��a@�,�@��@��v@���@�N�@�oi@�@�Mj@��!@��z@��@���@�a�@�E9@��4@��D@���@�O�@�+@�6@���@���@���@���@���@�a�@�S@��z@���@�k�@�O�@��B@�8�@��-@��K@�ߤ@��p@��$@�c�@��@��Q@�&@���@�Q@���@��*@�u�@��@��@�($@���@��@�O@�&@�@�7�@�y�@�5�@��@���@�p;@��@���@�7L@���@��@�U2@�Ft@���@�$t@��2@���@��x@��D@�K^@��@��k@�)_@��]@�h�@��A@�_p@��@��e@��b@���@�E�@��6@�5�@��@��?@�H@�	@���@��z@�|@�E9@�,�@�"�@��5@��h@� �@�zx@��@�r�@�$�@��@��H@��"@�O�@��@��L@�Z@�!�@�	�@�خ@���@�m]@�B�@��@��c@��6@�Ft@��@��a@��@�!�@���@��+@�a|@�4n@���@���@�u�@�)_@��@��x@�z�@�Q@�ϫ@�2a@��	@���@�h�@�6@��g@��4@� \@��@��@�8�@��@���@��@���@��a@�t�@�G�@��@�L0@��H@�Vm@�-w@��v@��o@�?�@�1�@��@��@���@�A�@���@��Y@�c�@�B[@��)@��n@���@�Vm@�-w@�!�@��@��E@���@�A�@�(�@��@���@�qv@�_p@�O@�@O@�4�@��@��@�ѷ@���@���@��@�u�@�@�@��.@��*@�e,@�0�@��<@�N�@�,=@�@��@�� @��t@�e�@�4�@� i@��/@��e@�z�@�L0@�7�@�A�@�l"@�h�@��@��@���@���@��@�GE@�{@��@
=@~�!@~��@~R�@~?@}�@}5�@|bN@{��@{�@{�@z��@y�T@y�@x��@w��@w&@v�@v?@u@uIR@t��@t��@t$@s� @s��@sv`@sb�@r��@q@q!�@p�D@pA�@o��@o�{@o�*@odZ@n��@m@l�e@k�r@k��@j�2@j\�@je@i�S@i<6@h�v@h��@hFt@hM@g�@gE9@g�@f�!@fi�@fE�@e��@eB�@d�`@d�z@dw�@dK^@c�g@c�@b�'@bL0@a��@ae,@a/@``�@_�Q@_8@^�s@^��@^GE@]�@]c�@\ی@\��@\��@\z�@\7�@[��@[�@Z�@Z}V@ZTa@Z{@Y�M@X�@XPH@X%�@W�
@W��@V�@VGE@UG�@T�@T�p@T~(@S��@R��@Ra|@R
�@Q��@P��@O�@OE9@N�@Nn�@M�n@Ma�@M�@L�z@L7�@K��@Kqv@K4�@K$t@J�8@J��@J�@JE�@I�D@I�@I�S@I \@H�@G��@G�@Gl�@F�@F�+@F-@E��@E��@E��@E�~@EY�@EN<@E%F@D�E@DD�@C�0@C/�@CS@B�@Bs�@B�@A��@Am]@AVm@AG�@A*0@A�@@�/@@��@@m�@@A�@?�Q@?{J@?X�@?S�@?�@>�'@>�h@>��@>M�@> �@=��@=F@=�@<�j@<q@<�@;˒@;��@;��@;�@:�b@:B[@:J@9�@9��@9s�@9B�@9&�@8��@8�E@8��@8��@8h�@8(�@7�+@7�@7_p@6��@6��@6^5@6-@5��@5��@5a�@5B�@4��@4	�@3��@3�F@3�:@3RT@3�@2�c@2��@2��@2�A@2H�@2�@1�z@1c@1rG@1c�@1/@1�@0�5@0��@0,=@0�@/��@/\)@.�@.��@.^5@.6�@.	@-��@-S&@-�@,֡@,��@,9X@+�r@+��@+��@+Mj@+$t@+�@*��@*ff@*@�@*_@)ϫ@)��@)	l@(�p@(��@(7�@'��@'{J@'F�@'Y@&��@&��@&��@&p;@&^5@&H�@&{@%�T@%��@%��@%8�@$��@$֡@$��@$�@$tT@$`�@$>B@$!@$�@$�@#�@#�@#��@#�*@#j�@#O@#�@"��@"($@!�.@!�D@!�)@!�@!@!��@!5�@!�@!�@!	l@ �f@ Ɇ@ ��@ H@�@33@��@��@v�@d�@\�@Ov@8�@�@u@�T@��@��@�@O�@�@��@��@c�@D�@~@1@��@�q@A�@"�@(@�2@��@�m@��@s�@Ta@3�@�.@�@��@^�@�@��@��@r�@7�@7@��@�@�{@J#@Y@�y@�m@�h@�\@��@kQ@
�@�@�t@��@a�@q@�@�@��@��@q@]d@?�@��@�F@�@@\)@)_@�@ i@�@�c@ߤ@��@�m@�6@}V@E�@)�@�@�@��@��@��@N<@-w@V@;@��@֡@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�e�A�e�A�f�A�iyA�kQA�m]A�n�A�n�A�p�A�qA�qA�qA�qAA�o�A�m�A�o�A�qvA�n�A�j�A�h�A�d�A�g8A�e�A�XyA�B[A�9�A�7�A��Aׯ�A�c�A��A��KA���A��A�{JA�1'AЍ�A˛qA�.A���A��AǡbAƼ�AĠ'A��?A�v�A�a�A�$A�'RA�*�A�U2A�)*A�O�A���A�ҽA�:�A���A�ƨA�:A���A��A�%zA���A���A�A��iA��#A�X�A�z�A��A�5tA���A�FA���A�o�A���A���A�XEA�r�A�J#A��A�+�A�7A��DA~�;A{��At�)Ao��AmcAm�Ak��Af�Ac��A_��A[�AW��AV�AR��AP�AMk�AIW?AC{A=��A:�0A8�A8?�A7OA5��A5�A4�A3�A3L0A2��A/^�A.�A+�'A(� A&�MA&6zA%zxA$��A% \A$�XA$e�A#��A"�;A"9�A!/�A �@A YKA�"A
�A�Ax�Aw2A;�AoA�^A�A�A��A�_A�$A�AJ�AFA�hA��A��A?�A-�A�A�A��A��Af�A��A�`A6�Ap;AeA�	A�A~Ao�A��AԕAi�Ah�AQ�A�A�dA��A�XA�IAHA�uA�A�A��A�MA,=A#�A�A��Ay�A2�A
��A
��A
�A	�A	��A	=�A�A�*Ay>AV�A�+A8�AϫAq�Ax�A0UA�vA��A��Al�AN�A%A�A�XA�:A�Am]A  A��AS�A�xAS&A�A�A��A4�A�6A��Al�A4�A�A �8A �/A �XA �KA �'A �@�a@��@���@�n�@�I�@���@��P@�.I@��6@�
�@�Y�@���@��)@�z�@�}V@��@�ԕ@��@�$@�J#@�"h@�x�@�f�@�e�@��@�A@�@��@�\@�1�@��;@�hs@�PH@��@�`�@힄@�5�@빌@�J#@��@�(�@�+@�خ@�o@��|@�R@思@���@��p@�1'@㔯@��,@�`�@�J@�y�@�.I@�0U@߄M@�{J@�a@�v�@ݓ@�`B@��@�h�@��@ښ�@ڇ�@ڤ�@ڳh@گO@�GE@�4@�ƨ@�T�@���@�H�@�'R@��@�G@��3@�@�0U@��@��6@�@�b�@ԇ�@ӝ�@�Y@Ҟ@�h
@�'R@ѓ@� \@���@�}V@�1@ϝ�@�o�@�S&@�@��5@ΰ!@�>B@�˒@�s@�!�@̎�@�8�@��@˼�@�j@�*0@��@���@���@��@�ȴ@ʾ@ʕ�@�xl@��@�y>@���@��3@�;@�$�@���@��@žw@�rG@�ں@Ĭ@�Ov@�J�@¯O@��@�@�?@��a@�,�@��@��v@���@�N�@�oi@�@�Mj@��!@��z@��@���@�a�@�E9@��4@��D@���@�O�@�+@�6@���@���@���@���@���@�a�@�S@��z@���@�k�@�O�@��B@�8�@��-@��K@�ߤ@��p@��$@�c�@��@��Q@�&@���@�Q@���@��*@�u�@��@��@�($@���@��@�O@�&@�@�7�@�y�@�5�@��@���@�p;@��@���@�7L@���@��@�U2@�Ft@���@�$t@��2@���@��x@��D@�K^@��@��k@�)_@��]@�h�@��A@�_p@��@��e@��b@���@�E�@��6@�5�@��@��?@�H@�	@���@��z@�|@�E9@�,�@�"�@��5@��h@� �@�zx@��@�r�@�$�@��@��H@��"@�O�@��@��L@�Z@�!�@�	�@�خ@���@�m]@�B�@��@��c@��6@�Ft@��@��a@��@�!�@���@��+@�a|@�4n@���@���@�u�@�)_@��@��x@�z�@�Q@�ϫ@�2a@��	@���@�h�@�6@��g@��4@� \@��@��@�8�@��@���@��@���@��a@�t�@�G�@��@�L0@��H@�Vm@�-w@��v@��o@�?�@�1�@��@��@���@�A�@���@��Y@�c�@�B[@��)@��n@���@�Vm@�-w@�!�@��@��E@���@�A�@�(�@��@���@�qv@�_p@�O@�@O@�4�@��@��@�ѷ@���@���@��@�u�@�@�@��.@��*@�e,@�0�@��<@�N�@�,=@�@��@�� @��t@�e�@�4�@� i@��/@��e@�z�@�L0@�7�@�A�@�l"@�h�@��@��@���@���@��@�GE@�{@��@
=@~�!@~��@~R�@~?@}�@}5�@|bN@{��@{�@{�@z��@y�T@y�@x��@w��@w&@v�@v?@u@uIR@t��@t��@t$@s� @s��@sv`@sb�@r��@q@q!�@p�D@pA�@o��@o�{@o�*@odZ@n��@m@l�e@k�r@k��@j�2@j\�@je@i�S@i<6@h�v@h��@hFt@hM@g�@gE9@g�@f�!@fi�@fE�@e��@eB�@d�`@d�z@dw�@dK^@c�g@c�@b�'@bL0@a��@ae,@a/@``�@_�Q@_8@^�s@^��@^GE@]�@]c�@\ی@\��@\��@\z�@\7�@[��@[�@Z�@Z}V@ZTa@Z{@Y�M@X�@XPH@X%�@W�
@W��@V�@VGE@UG�@T�@T�p@T~(@S��@R��@Ra|@R
�@Q��@P��@O�@OE9@N�@Nn�@M�n@Ma�@M�@L�z@L7�@K��@Kqv@K4�@K$t@J�8@J��@J�@JE�@I�D@I�@I�S@I \@H�@G��@G�@Gl�@F�@F�+@F-@E��@E��@E��@E�~@EY�@EN<@E%F@D�E@DD�@C�0@C/�@CS@B�@Bs�@B�@A��@Am]@AVm@AG�@A*0@A�@@�/@@��@@m�@@A�@?�Q@?{J@?X�@?S�@?�@>�'@>�h@>��@>M�@> �@=��@=F@=�@<�j@<q@<�@;˒@;��@;��@;�@:�b@:B[@:J@9�@9��@9s�@9B�@9&�@8��@8�E@8��@8��@8h�@8(�@7�+@7�@7_p@6��@6��@6^5@6-@5��@5��@5a�@5B�@4��@4	�@3��@3�F@3�:@3RT@3�@2�c@2��@2��@2�A@2H�@2�@1�z@1c@1rG@1c�@1/@1�@0�5@0��@0,=@0�@/��@/\)@.�@.��@.^5@.6�@.	@-��@-S&@-�@,֡@,��@,9X@+�r@+��@+��@+Mj@+$t@+�@*��@*ff@*@�@*_@)ϫ@)��@)	l@(�p@(��@(7�@'��@'{J@'F�@'Y@&��@&��@&��@&p;@&^5@&H�@&{@%�T@%��@%��@%8�@$��@$֡@$��@$�@$tT@$`�@$>B@$!@$�@$�@#�@#�@#��@#�*@#j�@#O@#�@"��@"($@!�.@!�D@!�)@!�@!@!��@!5�@!�@!�@!	l@ �f@ Ɇ@ ��@ H@�@33@��@��@v�@d�@\�@Ov@8�@�@u@�T@��@��@�@O�@�@��@��@c�@D�@~@1@��@�q@A�@"�@(@�2@��@�m@��@s�@Ta@3�@�.@�@��@^�@�@��@��@r�@7�@7@��@�@�{@J#@Y@�y@�m@�h@�\@��@kQ@
�@�@�t@��@a�@q@�@�@��@��@q@]d@?�@��@�F@�@@\)@)_@�@ i@�@�c@ߤ@��@�m@�6@}V@E�@)�@�@�@��@��@��@N<@-w@V@;@��@֡@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	�DB	�B	�DB	�yB	��B	��B	��B	��B	��B	��B	��B	�sB	�
B	�>B	��B	��B	�mB	�RB	�B	�B	�B	��B	�zB	��B	�ZB	�NB	��B	��B	m�B	h>B	f�B	e�B	bB	Z7B	R:B	0�B	.B	0�B	6zB	8�B	>BB	YB	lB	n}B	��B	�tB	��B	�4B	��B	��B
-�B
MB
_!B
X�B
cnB
r�B
��B
�9B
�-B
��B
�dB
��B�B�ByBB;B)DB�B
��B
��B
��B
�DB
}�B
l�B
C�B
�B	�B	�JB	��B	�OB	��B	��B	�MB	e�B	W�B	N�B	I�B	@�B	/�B	&B	�B	B	�B	oB	B	�B�+B��B��B׍B�+B�BևB�_BیB��B�cB��B�B	jB	�B	,"B	�B	B��B	�B�cB��B	QB	%�B	,B	5?B	;dB	AoB	L~B	RoB	V�B	\�B	_�B	gmB	p�B	zB	|�B	�'B	�%B	�XB	��B	��B	��B	�QB	�kB	��B	�)B	�|B	��B	��B	��B	��B	�=B	�B	�B	��B	��B	B	��B	�0B	�7B	ǮB	�B	��B	�qB	��B	��B	��B	��B	�B	ÖB	�mB	āB	��B	ǔB	�_B	ǮB	� B	�dB	�B	��B	B	��B	�B	˒B	��B	ΥB	��B	ΊB	�jB	�^B	�DB	ΥB	�:B	ԕB	�2B	��B	�{B	�1B	�ZB	�B	��B	��B	�9B	�B	�B	�lB	�2B	�5B	��B	�3B	��B	�MB	��B	��B	��B	�	B	��B	�B	��B	�6B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�]B	��B	��B	�cB	�B
B
B
UB
�B
?B
DB
VB
�B
�B
	�B
�B
?B
mB
�B
	B
fB
?B
�B
�B
�B
�B
B
B
gB
9B
�B
�B
�B	��B	�qB	��B	��B	�XB	��B	�?B	��B	�B	�9B	�B	�FB	��B	��B	�B	��B	��B	�jB	�B	�B	��B	�rB	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�fB	��B	�$B	�rB	��B	�^B	�B	��B	��B	��B	�B	�B	��B	�B	��B	�PB	��B	��B	��B	�^B	��B	�B	��B	�*B	�xB	�xB	��B	�*B	�*B	�B	�B	��B	��B	��B	��B	��B	�^B	�^B	�xB	��B	�0B	�JB	��B	��B	�VB	��B	�wB
'B
fB
	7B
	lB
	�B
	B
�B
?B
�B
�B
 iB	�B	�HB
 �B
'B
gB
�B
+B
�B
�B
_B
fB
	RB
	7B
�B
�B
�B
mB
�B
�B
	RB
�B

�B

XB
�B
^B

�B

�B
JB
�B
�B
�B
0B
JB
dB
�B
B

�B

=B
	�B
	�B
	RB
�B
�B
+B
tB
tB
?B
YB
�B
�B
�B
�B
�B
tB
�B
zB
EB
�B
�B
zB
zB
�B
�B
�B
�B
zB
zB
+B
+B
KB
fB
�B
�B
�B
�B
	RB
	�B
	�B

rB

�B

rB

=B

�B
)B
^B
^B
xB
DB
�B
�B
�B
�B
�B
6B
�B
�B
\B
\B
\B
vB
B
�B
�B
�B
�B
 B
�B
 B
 B
hB
hB
4B
B
�B
B
hB
�B
HB
HB
�B
HB
�B
bB
}B
}B
HB
�B
�B
�B
 B
4B
NB
�B
�B
 B
�B
[B
&B
�B
�B
@B
�B
FB
aB
�B
B
�B
�B
9B
mB
$B

B
�B
?B
sB
�B
YB

B
$B
�B
yB
�B
QB
QB
�B
�B
WB
)B
�B
B
B
B
B
B
�B
B
�B
IB
�B
5B
OB
5B
�B
!B
�B
�B
 BB
 BB
!B
"�B
"hB
"B
"B
"�B
$B
$@B
$tB
%B
%�B
%�B
%�B
%�B
%zB
%zB
%�B
%�B
%�B
%�B
%�B
&LB
&B
%�B
&LB
&�B
'B
'B
'�B
'�B
(
B
(�B
)�B
)�B
)�B
*B
*0B
*�B
+B
+B
+6B
+6B
+�B
,"B
,B
,�B
-]B
.cB
/�B
/�B
.�B
./B
.�B
.�B
/�B
0B
0�B
0�B
1'B
1'B
1vB
2GB
3MB
3hB
4nB
5%B
5B
5�B
6zB
6�B
6�B
7�B
8RB
8RB
8�B
8�B
9>B
9XB
9>B
9rB
9�B
9�B
:DB
:^B
:DB
:�B
:�B
:�B
:�B
:�B
:B
:^B
;B
;�B
;�B
;�B
;�B
;B
;JB
;�B
<PB
<�B
=B
<�B
=B
=VB
=qB
=qB
=�B
>�B
>�B
>�B
?B
?cB
@OB
@�B
@�B
@�B
@�B
@�B
@�B
A�B
AoB
A�B
B�B
B�B
B�B
CGB
C�B
DB
D�B
ESB
E�B
E�B
GEB
GzB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
I7B
I7B
I�B
J=B
J�B
J�B
J�B
K)B
J�B
J�B
J�B
K�B
L0B
LdB
L~B
LdB
L�B
L~B
MB
NB
L�B
L�B
MB
M6B
M�B
M�B
NB
NVB
N�B
N�B
OB
OB
OB
O(B
OBB
OvB
O�B
O�B
O�B
O�B
PHB
P�B
Q�B
Q�B
RTB
R�B
SB
S[B
S�B
T,B
T{B
T{B
U2B
UMB
UMB
U2B
UMB
U2B
U2B
UgB
U�B
U�B
VSB
V�B
V�B
V�B
V�B
W
B
W$B
W?B
XB
X+B
W�B
W�B
X+B
X+B
X_B
X�B
X�B
X�B
YB
YeB
YKB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
[WB
[WB
[qB
[�B
\B
\�B
\�B
\�B
\�B
\xB
\xB
\�B
\�B
]B
]/B
]IB
]~B
]�B
]�B
^B
^B
^jB
^�B
^�B
_B
_!B
_VB
_pB
_pB
_�B
`\B
`\B
`vB
`�B
`�B
`�B
`�B
aB
a-B
a-B
aHB
aHB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bhB
bhB
b�B
b�B
c:B
cTB
c�B
c�B
c�B
d&B
d@B
dZB
d�B
d�B
eB
e,B
e,B
eFB
e�B
e�B
e�B
fB
f2B
fLB
ffB
f�B
f�B
gmB
gmB
g�B
g�B
h>B
hXB
h�B
h�B
h�B
iB
i*B
i*B
iDB
iDB
iyB
i�B
i�B
i�B
jB
j0B
jKB
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
kB
j�B
j�B
k6B
kkB
kkB
k�B
k�B
lWB
lqB
lqB
lqB
lqB
l�B
l�B
mB
m)B
mB
m)B
m)B
m]B
m)B
m�B
nB
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
pUB
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
v�B
v�B
v�B
v�B
w2B
wfB
wLB
wLB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
zB
z*B
z*B
zDB
zDB
zxB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	�DB	�B	�DB	�yB	��B	��B	��B	��B	��B	��B	��B	�sB	�
B	�>B	��B	��B	�mB	�RB	�B	�B	�B	��B	�zB	��B	�ZB	�NB	��B	��B	m�B	h>B	f�B	e�B	bB	Z7B	R:B	0�B	.B	0�B	6zB	8�B	>BB	YB	lB	n}B	��B	�tB	��B	�4B	��B	��B
-�B
MB
_!B
X�B
cnB
r�B
��B
�9B
�-B
��B
�dB
��B�B�ByBB;B)DB�B
��B
��B
��B
�DB
}�B
l�B
C�B
�B	�B	�JB	��B	�OB	��B	��B	�MB	e�B	W�B	N�B	I�B	@�B	/�B	&B	�B	B	�B	oB	B	�B�+B��B��B׍B�+B�BևB�_BیB��B�cB��B�B	jB	�B	,"B	�B	B��B	�B�cB��B	QB	%�B	,B	5?B	;dB	AoB	L~B	RoB	V�B	\�B	_�B	gmB	p�B	zB	|�B	�'B	�%B	�XB	��B	��B	��B	�QB	�kB	��B	�)B	�|B	��B	��B	��B	��B	�=B	�B	�B	��B	��B	B	��B	�0B	�7B	ǮB	�B	��B	�qB	��B	��B	��B	��B	�B	ÖB	�mB	āB	��B	ǔB	�_B	ǮB	� B	�dB	�B	��B	B	��B	�B	˒B	��B	ΥB	��B	ΊB	�jB	�^B	�DB	ΥB	�:B	ԕB	�2B	��B	�{B	�1B	�ZB	�B	��B	��B	�9B	�B	�B	�lB	�2B	�5B	��B	�3B	��B	�MB	��B	��B	��B	�	B	��B	�B	��B	�6B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�]B	��B	��B	�cB	�B
B
B
UB
�B
?B
DB
VB
�B
�B
	�B
�B
?B
mB
�B
	B
fB
?B
�B
�B
�B
�B
B
B
gB
9B
�B
�B
�B	��B	�qB	��B	��B	�XB	��B	�?B	��B	�B	�9B	�B	�FB	��B	��B	�B	��B	��B	�jB	�B	�B	��B	�rB	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�fB	��B	�$B	�rB	��B	�^B	�B	��B	��B	��B	�B	�B	��B	�B	��B	�PB	��B	��B	��B	�^B	��B	�B	��B	�*B	�xB	�xB	��B	�*B	�*B	�B	�B	��B	��B	��B	��B	��B	�^B	�^B	�xB	��B	�0B	�JB	��B	��B	�VB	��B	�wB
'B
fB
	7B
	lB
	�B
	B
�B
?B
�B
�B
 iB	�B	�HB
 �B
'B
gB
�B
+B
�B
�B
_B
fB
	RB
	7B
�B
�B
�B
mB
�B
�B
	RB
�B

�B

XB
�B
^B

�B

�B
JB
�B
�B
�B
0B
JB
dB
�B
B

�B

=B
	�B
	�B
	RB
�B
�B
+B
tB
tB
?B
YB
�B
�B
�B
�B
�B
tB
�B
zB
EB
�B
�B
zB
zB
�B
�B
�B
�B
zB
zB
+B
+B
KB
fB
�B
�B
�B
�B
	RB
	�B
	�B

rB

�B

rB

=B

�B
)B
^B
^B
xB
DB
�B
�B
�B
�B
�B
6B
�B
�B
\B
\B
\B
vB
B
�B
�B
�B
�B
 B
�B
 B
 B
hB
hB
4B
B
�B
B
hB
�B
HB
HB
�B
HB
�B
bB
}B
}B
HB
�B
�B
�B
 B
4B
NB
�B
�B
 B
�B
[B
&B
�B
�B
@B
�B
FB
aB
�B
B
�B
�B
9B
mB
$B

B
�B
?B
sB
�B
YB

B
$B
�B
yB
�B
QB
QB
�B
�B
WB
)B
�B
B
B
B
B
B
�B
B
�B
IB
�B
5B
OB
5B
�B
!B
�B
�B
 BB
 BB
!B
"�B
"hB
"B
"B
"�B
$B
$@B
$tB
%B
%�B
%�B
%�B
%�B
%zB
%zB
%�B
%�B
%�B
%�B
%�B
&LB
&B
%�B
&LB
&�B
'B
'B
'�B
'�B
(
B
(�B
)�B
)�B
)�B
*B
*0B
*�B
+B
+B
+6B
+6B
+�B
,"B
,B
,�B
-]B
.cB
/�B
/�B
.�B
./B
.�B
.�B
/�B
0B
0�B
0�B
1'B
1'B
1vB
2GB
3MB
3hB
4nB
5%B
5B
5�B
6zB
6�B
6�B
7�B
8RB
8RB
8�B
8�B
9>B
9XB
9>B
9rB
9�B
9�B
:DB
:^B
:DB
:�B
:�B
:�B
:�B
:�B
:B
:^B
;B
;�B
;�B
;�B
;�B
;B
;JB
;�B
<PB
<�B
=B
<�B
=B
=VB
=qB
=qB
=�B
>�B
>�B
>�B
?B
?cB
@OB
@�B
@�B
@�B
@�B
@�B
@�B
A�B
AoB
A�B
B�B
B�B
B�B
CGB
C�B
DB
D�B
ESB
E�B
E�B
GEB
GzB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
I7B
I7B
I�B
J=B
J�B
J�B
J�B
K)B
J�B
J�B
J�B
K�B
L0B
LdB
L~B
LdB
L�B
L~B
MB
NB
L�B
L�B
MB
M6B
M�B
M�B
NB
NVB
N�B
N�B
OB
OB
OB
O(B
OBB
OvB
O�B
O�B
O�B
O�B
PHB
P�B
Q�B
Q�B
RTB
R�B
SB
S[B
S�B
T,B
T{B
T{B
U2B
UMB
UMB
U2B
UMB
U2B
U2B
UgB
U�B
U�B
VSB
V�B
V�B
V�B
V�B
W
B
W$B
W?B
XB
X+B
W�B
W�B
X+B
X+B
X_B
X�B
X�B
X�B
YB
YeB
YKB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
[WB
[WB
[qB
[�B
\B
\�B
\�B
\�B
\�B
\xB
\xB
\�B
\�B
]B
]/B
]IB
]~B
]�B
]�B
^B
^B
^jB
^�B
^�B
_B
_!B
_VB
_pB
_pB
_�B
`\B
`\B
`vB
`�B
`�B
`�B
`�B
aB
a-B
a-B
aHB
aHB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bhB
bhB
b�B
b�B
c:B
cTB
c�B
c�B
c�B
d&B
d@B
dZB
d�B
d�B
eB
e,B
e,B
eFB
e�B
e�B
e�B
fB
f2B
fLB
ffB
f�B
f�B
gmB
gmB
g�B
g�B
h>B
hXB
h�B
h�B
h�B
iB
i*B
i*B
iDB
iDB
iyB
i�B
i�B
i�B
jB
j0B
jKB
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
kB
j�B
j�B
k6B
kkB
kkB
k�B
k�B
lWB
lqB
lqB
lqB
lqB
l�B
l�B
mB
m)B
mB
m)B
m)B
m]B
m)B
m�B
nB
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
pUB
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
q�B
r|B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
shB
shB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
vB
vB
v+B
v+B
v+B
v�B
v�B
v�B
v�B
w2B
wfB
wLB
wLB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
zB
z*B
z*B
zDB
zDB
zxB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192011  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192011  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192011                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042021  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042021  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                