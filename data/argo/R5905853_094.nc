CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:39:29Z creation;2022-06-04T17:39:29Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173929  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ^A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ِ��kU1   @ِ����j@-��t�j�cF$�/�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0��B7��B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�CL�C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CC�fCF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @
>@}p�@��R@��RA\)A?\)A]A\)A��A��A��A�z�AϮA߮A�A��B�
B�
B�
B�
B'�
B0��B7p�B?p�BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��RB��RB��B��RB��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C]CB�C�)C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CB]CC�)CE��CG��CI��CK��CM��CO�)CQ��CS��CU��CW��CY��C[��C]��C_�)Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cr]Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP��DQ�DQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDpwDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~��D~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�Dֻ�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�{�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�xA�
�A��oAٮ}A��EA�ںA�o�A�$@A��A��A�	�A�{A���A��A��	A��%A��A؞OA؃GA�cTAׅSA�=�AԵtA��A�+�AЫA�Z�A�5tA�)�A��A�F?A�{A�pA��JA�p;A�!�A��A�jA��A�33A�!-A���A�A�A�oA��~A�چA�7�A���A��9A���A��zA��A�fA�j�A��A�R�A�.A��PA�#:A�یA���A�J#A��lA�ܒA�c A�z�A�!-A��A�ҽA���A�A�g�A��GA�J�A�Q�A��_A�iyA���A���A�'�A�~�A��)A�DA��A��oA��A��A���A�ȴA��qA�v`A�{A��AA�(�A��A���A�~(A�%zA��zA}4�Ax9XAup�ApC�Ak� Ad8�A`�)A[Y�AY�AU�VAPiDAM+�AI�TAF�ACSAB/A@F�A?OA>҉A=/�A:��A98�A5�NA0��A-4A+q�A*�jA*�A+˒A,�A,L�A,�_A,�YA,�A*�A)�\A({�A(L0A'҉A&ںA%�XA%K�A%�*A%ѷA$�ZA$��A#N�A#m]A#YA"TaA!ȴA!�+A �zAR�Aa�A�]A�fA=qA�]A�*A&�AR�A��AɆAi�A�,Ap;A҉A\�A�A��A�.APHA�sA�A��A� AZAuA��A��AqA3�A�A�{A�A��A\)AVA�DA+A��A iAA��A�AA�A�1A��AJ�A
�'A	�NA	^�Al�A3�A/�A�<Al"A33A҉A�4As�A9XA�AxA5?A&�A�A�PA�NA4A �A ��A Vm@��$@�7@�@�@��@��H@�v`@�Y@�U2@��H@�q�@�Z�@�YK@�IR@�خ@�F@�1@�K�@�<@�4@�X�@�w�@�ݘ@�C@궮@�$@�=�@��@�%�@��@�o@�H@��Z@�'�@�G@�Mj@�GE@�T�@��@�r�@���@߱[@߄M@�\�@��@ް!@�n�@މ�@�GE@�RT@�$t@��@�bN@��@��@�y�@�ߤ@�v�@��T@ٲ-@�hs@��@��5@��p@ج�@�c�@��@׏�@�O�@ָR@�r�@�l�@�,=@�?}@ԫ6@�l"@��T@�(�@�͟@�p;@�hs@�C@�C@ϛ=@��@�m�@�e�@��@��z@�l�@���@ˑh@���@��]@Ʌ@�%F@�e�@�j�@�@@�M@��A@�.I@Ə\@�8�@���@�O@ķ�@�Z@�@���@Ý�@¿�@�@�@��@���@��	@��	@�u�@��W@�j�@�u%@�K^@�3�@��@�q�@���@��k@�O@�:�@�U�@�E9@��m@�|�@��@���@�X�@�&�@��j@�M@��@�s@�@���@�_�@�PH@��]@���@��-@�c@�33@���@��1@�GE@���@�}�@�X@��@��x@�w�@�V@��@�{J@�!�@��@�C-@��3@��@�($@�"�@���@�l�@��k@�)_@��_@��@��@��@��{@��@�kQ@�7@���@�k�@�b�@���@�1@��d@��+@���@�2�@�u%@�{@���@�[W@���@�;�@�˒@��	@��{@��@�V@��@�+�@��9@�w�@�Ta@�6@�M@���@�%F@��z@�3�@��@���@��@��@���@�_p@���@�M@���@��j@���@�5�@��@�($@��@�X�@���@���@��z@��r@�_@�-@��g@�|@�s@�dZ@�t�@�t�@�7L@�'�@�%F@�@��@��f@��@��s@���@�_�@�I�@�(�@�{@��m@���@��7@�\�@��@���@�,=@� �@��@��0@���@�v`@�Q�@�C@���@�Q�@��r@��@��f@�8@��M@�m�@��@���@�;d@��@��@��p@���@�GE@�&�@�Y�@��@���@���@�w�@�/�@���@��t@�j@��@��@���@�h
@���@���@���@�N<@�&�@�@��@��I@�l�@�Z�@�M@���@�w2@�X�@�/�@��@��)@��m@���@�s�@���@���@��~@�Dg@�%F@��@� i@��@��!@��@��u@�tT@�_@�I�@�4n@�e@�J@��o@��z@��@�X�@��@���@���@�U2@�@��@�0@_p@~��@~��@~H�@}�T@}�'@}	l@|��@|7�@{�*@{�:@{qv@{ i@zp;@y��@yu�@y!�@xq@w�6@w��@wa@wU�@v�\@v5?@v{@u��@t��@tbN@t�@s�6@sS�@r�M@r�6@rYK@q��@q�j@q|@qq@pg8@pG@oO@n�1@n($@m��@m�@mY�@lѷ@lu�@lM@k�m@kO@j�@j�@i��@i�N@i:�@h1'@gO@f�\@f+k@e�j@e`B@d�|@d��@d~@c�k@cy�@cZ�@c i@bl�@b@a�3@a�@a�@`��@_��@_�@^�@^��@^��@^�r@]��@]B�@\u�@\I�@\C-@\@[�$@[e�@[U�@Z��@Zl�@Y�Z@YO�@X��@X�@Xm�@W��@W_p@W!-@V~�@U��@U�h@Uf�@T��@Ty>@S�@S|�@SY@Rߤ@Ru%@Rd�@ROv@R@Q��@P��@P�u@P@O��@O\)@O"�@N�2@N�b@N!�@M�@L��@LH@K��@K��@Kn/@J�L@Js�@J1�@I�j@H��@H�E@H��@H?�@G�]@G��@G��@G��@G)_@F��@F�!@Fi�@E�.@E��@E�n@Em]@E�@D�e@Dg8@D,=@D	�@C�m@C�[@C=@B�\@B-@B$�@B@Aϫ@A�X@AS&@@Ĝ@@<�@@$@?�a@?�@?P�@>��@>=q@=��@=<6@<֡@<K^@;��@;�@@;n/@;!-@:�@:ff@9�.@9��@9�S@9Y�@8֡@8��@8bN@7�r@7��@7�@7�P@6��@6Ta@5S&@5/@5�@4��@4H@3�*@2�y@2�1@2d�@21�@2J@1�j@1�~@1%@0��@0��@0e�@0Ft@0%�@/�r@/��@/��@/l�@/.I@.�]@.�R@.q�@.a|@.Z�@.E�@.	@-��@-L�@,��@,��@,I�@+�+@+��@+S�@*�]@*�h@*�}@*�r@*c @*$�@)�@)Vm@)�@(�[@(�j@(��@(2�@(G@'� @'��@'��@'\)@'.I@'�@&�x@&Ta@&=q@&.�@&�@%�d@%�C@%s�@%�@$�@$�E@$��@$�@$u�@$[�@$~@#�@#��@#�$@#x@#g�@#RT@#33@#�@"��@"Ta@"$�@!�@!��@!@ ��@ h�@�@��@�6@�6@�0@t�@6z@�s@��@YK@�@4@��@�t@x�@X@&�@�	@��@U2@@��@��@t�@]�@A�@�@�h@��@s�@=q@J@��@��@�~@�@��@<�@�@�Q@�V@�	@��@�@X�@�@�@�z@��@+�@�f@��@��@[�@1'@��@�}@�f@o�@@O@�@҉@�r@c @Q@5?@
�@��@Y�@%F@��@�j@�@h�@?�@2�@ �@�@�@�Q@˒@�@��@�P@F�@�@�c@�@�}@}V@u%@R�@�@�3@|@Y�@<6@��@�?@�D@��@e�@Ft@>B@b@�a@�@@t�@;d@
�@
l�@
$�@	�j@	�d@	��@	`B@	IR@	8�@	#�@	�@	+@	�@��@Ĝ@�o@oi@[�@C-@-�@b@��@��@��@�f@�4@�4@|�@t�@j�@n/@a@A�@�@��@�H@��@�@Q@@�@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�xA�
�A��oAٮ}A��EA�ںA�o�A�$@A��A��A�	�A�{A���A��A��	A��%A��A؞OA؃GA�cTAׅSA�=�AԵtA��A�+�AЫA�Z�A�5tA�)�A��A�F?A�{A�pA��JA�p;A�!�A��A�jA��A�33A�!-A���A�A�A�oA��~A�چA�7�A���A��9A���A��zA��A�fA�j�A��A�R�A�.A��PA�#:A�یA���A�J#A��lA�ܒA�c A�z�A�!-A��A�ҽA���A�A�g�A��GA�J�A�Q�A��_A�iyA���A���A�'�A�~�A��)A�DA��A��oA��A��A���A�ȴA��qA�v`A�{A��AA�(�A��A���A�~(A�%zA��zA}4�Ax9XAup�ApC�Ak� Ad8�A`�)A[Y�AY�AU�VAPiDAM+�AI�TAF�ACSAB/A@F�A?OA>҉A=/�A:��A98�A5�NA0��A-4A+q�A*�jA*�A+˒A,�A,L�A,�_A,�YA,�A*�A)�\A({�A(L0A'҉A&ںA%�XA%K�A%�*A%ѷA$�ZA$��A#N�A#m]A#YA"TaA!ȴA!�+A �zAR�Aa�A�]A�fA=qA�]A�*A&�AR�A��AɆAi�A�,Ap;A҉A\�A�A��A�.APHA�sA�A��A� AZAuA��A��AqA3�A�A�{A�A��A\)AVA�DA+A��A iAA��A�AA�A�1A��AJ�A
�'A	�NA	^�Al�A3�A/�A�<Al"A33A҉A�4As�A9XA�AxA5?A&�A�A�PA�NA4A �A ��A Vm@��$@�7@�@�@��@��H@�v`@�Y@�U2@��H@�q�@�Z�@�YK@�IR@�خ@�F@�1@�K�@�<@�4@�X�@�w�@�ݘ@�C@궮@�$@�=�@��@�%�@��@�o@�H@��Z@�'�@�G@�Mj@�GE@�T�@��@�r�@���@߱[@߄M@�\�@��@ް!@�n�@މ�@�GE@�RT@�$t@��@�bN@��@��@�y�@�ߤ@�v�@��T@ٲ-@�hs@��@��5@��p@ج�@�c�@��@׏�@�O�@ָR@�r�@�l�@�,=@�?}@ԫ6@�l"@��T@�(�@�͟@�p;@�hs@�C@�C@ϛ=@��@�m�@�e�@��@��z@�l�@���@ˑh@���@��]@Ʌ@�%F@�e�@�j�@�@@�M@��A@�.I@Ə\@�8�@���@�O@ķ�@�Z@�@���@Ý�@¿�@�@�@��@���@��	@��	@�u�@��W@�j�@�u%@�K^@�3�@��@�q�@���@��k@�O@�:�@�U�@�E9@��m@�|�@��@���@�X�@�&�@��j@�M@��@�s@�@���@�_�@�PH@��]@���@��-@�c@�33@���@��1@�GE@���@�}�@�X@��@��x@�w�@�V@��@�{J@�!�@��@�C-@��3@��@�($@�"�@���@�l�@��k@�)_@��_@��@��@��@��{@��@�kQ@�7@���@�k�@�b�@���@�1@��d@��+@���@�2�@�u%@�{@���@�[W@���@�;�@�˒@��	@��{@��@�V@��@�+�@��9@�w�@�Ta@�6@�M@���@�%F@��z@�3�@��@���@��@��@���@�_p@���@�M@���@��j@���@�5�@��@�($@��@�X�@���@���@��z@��r@�_@�-@��g@�|@�s@�dZ@�t�@�t�@�7L@�'�@�%F@�@��@��f@��@��s@���@�_�@�I�@�(�@�{@��m@���@��7@�\�@��@���@�,=@� �@��@��0@���@�v`@�Q�@�C@���@�Q�@��r@��@��f@�8@��M@�m�@��@���@�;d@��@��@��p@���@�GE@�&�@�Y�@��@���@���@�w�@�/�@���@��t@�j@��@��@���@�h
@���@���@���@�N<@�&�@�@��@��I@�l�@�Z�@�M@���@�w2@�X�@�/�@��@��)@��m@���@�s�@���@���@��~@�Dg@�%F@��@� i@��@��!@��@��u@�tT@�_@�I�@�4n@�e@�J@��o@��z@��@�X�@��@���@���@�U2@�@��@�0@_p@~��@~��@~H�@}�T@}�'@}	l@|��@|7�@{�*@{�:@{qv@{ i@zp;@y��@yu�@y!�@xq@w�6@w��@wa@wU�@v�\@v5?@v{@u��@t��@tbN@t�@s�6@sS�@r�M@r�6@rYK@q��@q�j@q|@qq@pg8@pG@oO@n�1@n($@m��@m�@mY�@lѷ@lu�@lM@k�m@kO@j�@j�@i��@i�N@i:�@h1'@gO@f�\@f+k@e�j@e`B@d�|@d��@d~@c�k@cy�@cZ�@c i@bl�@b@a�3@a�@a�@`��@_��@_�@^�@^��@^��@^�r@]��@]B�@\u�@\I�@\C-@\@[�$@[e�@[U�@Z��@Zl�@Y�Z@YO�@X��@X�@Xm�@W��@W_p@W!-@V~�@U��@U�h@Uf�@T��@Ty>@S�@S|�@SY@Rߤ@Ru%@Rd�@ROv@R@Q��@P��@P�u@P@O��@O\)@O"�@N�2@N�b@N!�@M�@L��@LH@K��@K��@Kn/@J�L@Js�@J1�@I�j@H��@H�E@H��@H?�@G�]@G��@G��@G��@G)_@F��@F�!@Fi�@E�.@E��@E�n@Em]@E�@D�e@Dg8@D,=@D	�@C�m@C�[@C=@B�\@B-@B$�@B@Aϫ@A�X@AS&@@Ĝ@@<�@@$@?�a@?�@?P�@>��@>=q@=��@=<6@<֡@<K^@;��@;�@@;n/@;!-@:�@:ff@9�.@9��@9�S@9Y�@8֡@8��@8bN@7�r@7��@7�@7�P@6��@6Ta@5S&@5/@5�@4��@4H@3�*@2�y@2�1@2d�@21�@2J@1�j@1�~@1%@0��@0��@0e�@0Ft@0%�@/�r@/��@/��@/l�@/.I@.�]@.�R@.q�@.a|@.Z�@.E�@.	@-��@-L�@,��@,��@,I�@+�+@+��@+S�@*�]@*�h@*�}@*�r@*c @*$�@)�@)Vm@)�@(�[@(�j@(��@(2�@(G@'� @'��@'��@'\)@'.I@'�@&�x@&Ta@&=q@&.�@&�@%�d@%�C@%s�@%�@$�@$�E@$��@$�@$u�@$[�@$~@#�@#��@#�$@#x@#g�@#RT@#33@#�@"��@"Ta@"$�@!�@!��@!@ ��@ h�@�@��@�6@�6@�0@t�@6z@�s@��@YK@�@4@��@�t@x�@X@&�@�	@��@U2@@��@��@t�@]�@A�@�@�h@��@s�@=q@J@��@��@�~@�@��@<�@�@�Q@�V@�	@��@�@X�@�@�@�z@��@+�@�f@��@��@[�@1'@��@�}@�f@o�@@O@�@҉@�r@c @Q@5?@
�@��@Y�@%F@��@�j@�@h�@?�@2�@ �@�@�@�Q@˒@�@��@�P@F�@�@�c@�@�}@}V@u%@R�@�@�3@|@Y�@<6@��@�?@�D@��@e�@Ft@>B@b@�a@�@@t�@;d@
�@
l�@
$�@	�j@	�d@	��@	`B@	IR@	8�@	#�@	�@	+@	�@��@Ĝ@�o@oi@[�@C-@-�@b@��@��@��@�f@�4@�4@|�@t�@j�@n/@a@A�@�@��@�H@��@�@Q@@�@@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BvBvBBBB�BpBpB~BJB~B�B6BjB�B�BpB�B-CBC�BH�B>(BQBWBB
�B
��B
��B
��B
��B
�DB
��B
��B
�>B
�WB
өB
��B
�'B
�
B
�=B
�@B
�&B
�aB
�B�B@iBe,BpUB�uB��B��B�B�B��B��B�fB�\B��B�-B��B��B��B�B�BB��B��B�TB�rB�B�B�&B��B�KB�B�
B��B�B�.B��B�B�?B�)B��B��B�IB�mB�\B�gB�iB�=B~BcnBQ B5tBdB
��B
��B
O�B
 BB
 �B	�XB	�pB	�qB	�NB	��B	k�B	^B	P�B	O�B	9>B	�B	B	
	B		7B		�B	mB	}B	"�B	$tB	0�B	6�B	E9B	3B	�B	"�B	�B	B	5�B	_B	jeB	zxB	��B	��B	�bB	�;B	��B	��B	�B	��B	��B	��B	�.B	��B	�B	�BB	ܬB	ؓB	�B	��B	�B	��B	�B	�qB	�B	�B	��B	��B	�B	��B	��B	�nB	�'B	�dB	�B	��B	��B	�EB	�B	�B	��B	�RB	�YB	ÖB	�aB	��B	��B	�B	��B	�uB	��B	��B	�B	ðB	�{B	āB	��B	�?B	ƎB	��B	�+B	�_B	ɠB	οB	յB	�9B	�
B	�TB	�JB	�yB	ؓB	�9B	�,B	ңB	��B	�B	�1B	��B	ŢB	�fB	��B	��B	�B	�B	�?B	�B	��B	��B	�fB	�KB	��B	ĶB	��B	�tB	ȚB	ɠB	�1B	��B	�tB	��B	��B	ĶB	�3B	��B	�{B	āB	��B	ȚB	ȀB	�#B	�=B	�lB	�rB	�B	˒B	�xB	�xB	��B	�dB	�0B	�~B	�6B	��B	��B	͹B	��B	�<B	͹B	̘B	�0B	�xB	�~B	�B	̈́B	ΊB	��B	� B	ңB	��B	�@B	�B	�[B	�MB	�MB	�B	ԕB	ԕB	өB	��B	յB	�mB	�$B	�_B	רB	�sB	�EB	�+B	�B	یB	�xB	ܒB	�xB	�	B	�xB	�]B	��B	�B	�VB	޸B	�B	�bB	� B	�nB	�B	�-B	��B	�jB	�B	�B	�]B	�xB	�dB	��B	��B	�!B	�!B	�HB	��B	�vB	�bB	�B	�B	�DB	�kB	��B	�QB	�B	��B	��B	��B	�)B	��B	� B	�;B	�UB	�B	�B	�B	��B	��B	�B	�RB	��B	�B	�	B	��B	�B	��B	��B	��B	�>B	�^B	��B	�qB	�cB	�}B
 B
 B
 OB
 �B
 �B
 �B
B
 B
�B
�B
�B
�B
uB
�B
aB
GB
GB
aB
�B
B
�B
�B
3B
�B
gB
�B
�B
�B
�B
�B
�B
SB
B
�B
�B
[B
 �B	�]B	�qB	��B	�BB	��B	�]B	�<B	��B	��B	�B	��B	�0B	��B	�JB	�0B	��B	�0B	�dB	�B	�B	��B
;B
B
9B
B
�B
uB
'B
�B
�B
3B
EB
zB
�B
zB
�B
1B
�B
�B
�B
�B
fB
�B
zB
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
EB
�B
�B
�B
�B
�B
+B
_B
B
	�B

=B

�B
B

�B
�B
PB
�B
�B
B
�B
NB
B
�B
�B
sB
�B
+B
�B
kB
#B
#B
�B
CB
xB
�B
B
IB
�B
B
5B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 vB
 �B
 �B
!�B
!HB
"NB
#nB
#nB
#nB
#�B
#�B
"�B
#�B
$�B
$�B
%B
%,B
$�B
$�B
%FB
%,B
%zB
%�B
%�B
%zB
&B
&2B
%�B
%�B
&�B
&fB
&fB
&�B
'B
&�B
&�B
'RB
(XB
)*B
)DB
)*B
)B
(�B
(�B
)yB
)�B
*�B
*�B
*�B
+kB
+kB
+�B
+�B
,B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.IB
.�B
/5B
/�B
0UB
1'B
1�B
1�B
1�B
1�B
2B
1�B
2GB
2GB
2�B
2�B
3�B
3�B
3�B
4nB
49B
49B
4�B
4�B
4�B
5%B
4�B
4nB
4�B
4�B
5%B
5�B
6+B
6FB
6FB
7fB
7�B
7�B
8B
8RB
88B
8lB
8lB
8lB
7�B
8�B
9�B
9�B
9�B
9�B
:^B
;�B
;�B
<PB
<jB
<�B
<�B
=<B
=qB
=qB
=�B
>]B
>�B
>�B
>�B
>�B
?}B
?�B
@OB
@�B
@�B
@�B
AB
A;B
A;B
AoB
AoB
AUB
A�B
BB
BAB
BuB
B[B
B�B
B�B
C�B
C�B
D3B
D3B
DB
D3B
D�B
E9B
E�B
E�B
E�B
E�B
FtB
FtB
FYB
F�B
GEB
G�B
HB
HKB
H�B
HfB
H�B
H�B
IB
I�B
I�B
J#B
J	B
J�B
J�B
KDB
K�B
L0B
LdB
L�B
L�B
L�B
L�B
L�B
MjB
M�B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
O�B
PHB
P�B
P�B
Q B
QB
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
S&B
S&B
S[B
S&B
SuB
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
UB
UMB
UMB
U�B
U�B
U�B
VB
VB
UMB
U�B
V�B
VmB
VSB
VB
U�B
U�B
U�B
V9B
W
B
V�B
W?B
WsB
W?B
W?B
V�B
V�B
W
B
W
B
W�B
XB
W�B
XEB
X�B
Y1B
YeB
Y�B
ZB
ZkB
Y�B
ZQB
ZQB
ZB
ZB
Z�B
Z�B
[�B
\�B
\xB
\]B
\�B
\xB
\�B
]�B
^B
^B
^5B
^5B
^jB
^�B
_;B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`vB
`�B
a-B
aHB
a|B
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
c�B
c�B
c�B
dtB
d�B
eB
eFB
ezB
e�B
e�B
fB
f�B
f�B
gB
gB
gmB
g�B
g�B
h$B
h
B
h>B
h>B
hXB
hsB
h�B
i_B
iDB
iyB
iDB
i�B
i�B
i�B
jKB
j�B
jeB
j�B
j�B
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lB
l�B
l�B
mB
l�B
m)B
m)B
mwB
mCB
m)B
m)B
mwB
mCB
m)B
mCB
mCB
m]B
mwB
mwB
m�B
m�B
nB
n/B
n}B
n�B
n�B
o B
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
p�B
q�B
q�B
rB
q�B
r-B
r�B
shB
s�B
s�B
s�B
tB
tB
s�B
s�B
s�B
t9B
uZB
uB
u?B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
w�B
w�B
xB
xB
xlB
x�B
x�B
y	B
yXB
y�B
y�B
y�B
z*B
z�B
z�B
{0B
{�B
|B
|B
|PB
|�B
|�B
}"B
}<B
}�B
}�B
}qB
~B
~B
~]B
~(B
~�B
~�B
~�B
B
B
}B
�B
� B
�B
�iB
�OB
��B
��B
��B
��B
��B
�B
�oB
�oB
�oB
��B
�AB
��B
��B
�B
�B
�GB
��B
��B
��B
�B
��B
�3B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�9B
�B
�mB
��B
��B
�mB
��B
�mB
�mB
�SB
�mB
�mB
��B
��B
�B
�B
�tB
��B
��B
�Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BvBvBBBB�BpBpB~BJB~B�B6BjB�B�BpB�B-CBC�BH�B>(BQBWBB
�B
��B
��B
��B
��B
�DB
��B
��B
�>B
�WB
өB
��B
�'B
�
B
�=B
�@B
�&B
�aB
�B�B@iBe,BpUB�uB��B��B�B�B��B��B�fB�\B��B�-B��B��B��B�B�BB��B��B�TB�rB�B�B�&B��B�KB�B�
B��B�B�.B��B�B�?B�)B��B��B�IB�mB�\B�gB�iB�=B~BcnBQ B5tBdB
��B
��B
O�B
 BB
 �B	�XB	�pB	�qB	�NB	��B	k�B	^B	P�B	O�B	9>B	�B	B	
	B		7B		�B	mB	}B	"�B	$tB	0�B	6�B	E9B	3B	�B	"�B	�B	B	5�B	_B	jeB	zxB	��B	��B	�bB	�;B	��B	��B	�B	��B	��B	��B	�.B	��B	�B	�BB	ܬB	ؓB	�B	��B	�B	��B	�B	�qB	�B	�B	��B	��B	�B	��B	��B	�nB	�'B	�dB	�B	��B	��B	�EB	�B	�B	��B	�RB	�YB	ÖB	�aB	��B	��B	�B	��B	�uB	��B	��B	�B	ðB	�{B	āB	��B	�?B	ƎB	��B	�+B	�_B	ɠB	οB	յB	�9B	�
B	�TB	�JB	�yB	ؓB	�9B	�,B	ңB	��B	�B	�1B	��B	ŢB	�fB	��B	��B	�B	�B	�?B	�B	��B	��B	�fB	�KB	��B	ĶB	��B	�tB	ȚB	ɠB	�1B	��B	�tB	��B	��B	ĶB	�3B	��B	�{B	āB	��B	ȚB	ȀB	�#B	�=B	�lB	�rB	�B	˒B	�xB	�xB	��B	�dB	�0B	�~B	�6B	��B	��B	͹B	��B	�<B	͹B	̘B	�0B	�xB	�~B	�B	̈́B	ΊB	��B	� B	ңB	��B	�@B	�B	�[B	�MB	�MB	�B	ԕB	ԕB	өB	��B	յB	�mB	�$B	�_B	רB	�sB	�EB	�+B	�B	یB	�xB	ܒB	�xB	�	B	�xB	�]B	��B	�B	�VB	޸B	�B	�bB	� B	�nB	�B	�-B	��B	�jB	�B	�B	�]B	�xB	�dB	��B	��B	�!B	�!B	�HB	��B	�vB	�bB	�B	�B	�DB	�kB	��B	�QB	�B	��B	��B	��B	�)B	��B	� B	�;B	�UB	�B	�B	�B	��B	��B	�B	�RB	��B	�B	�	B	��B	�B	��B	��B	��B	�>B	�^B	��B	�qB	�cB	�}B
 B
 B
 OB
 �B
 �B
 �B
B
 B
�B
�B
�B
�B
uB
�B
aB
GB
GB
aB
�B
B
�B
�B
3B
�B
gB
�B
�B
�B
�B
�B
�B
SB
B
�B
�B
[B
 �B	�]B	�qB	��B	�BB	��B	�]B	�<B	��B	��B	�B	��B	�0B	��B	�JB	�0B	��B	�0B	�dB	�B	�B	��B
;B
B
9B
B
�B
uB
'B
�B
�B
3B
EB
zB
�B
zB
�B
1B
�B
�B
�B
�B
fB
�B
zB
�B
�B
�B
�B
�B
�B
_B
�B
�B
�B
EB
�B
�B
�B
�B
�B
+B
_B
B
	�B

=B

�B
B

�B
�B
PB
�B
�B
B
�B
NB
B
�B
�B
sB
�B
+B
�B
kB
#B
#B
�B
CB
xB
�B
B
IB
�B
B
5B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 vB
 �B
 �B
!�B
!HB
"NB
#nB
#nB
#nB
#�B
#�B
"�B
#�B
$�B
$�B
%B
%,B
$�B
$�B
%FB
%,B
%zB
%�B
%�B
%zB
&B
&2B
%�B
%�B
&�B
&fB
&fB
&�B
'B
&�B
&�B
'RB
(XB
)*B
)DB
)*B
)B
(�B
(�B
)yB
)�B
*�B
*�B
*�B
+kB
+kB
+�B
+�B
,B
,�B
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.IB
.�B
/5B
/�B
0UB
1'B
1�B
1�B
1�B
1�B
2B
1�B
2GB
2GB
2�B
2�B
3�B
3�B
3�B
4nB
49B
49B
4�B
4�B
4�B
5%B
4�B
4nB
4�B
4�B
5%B
5�B
6+B
6FB
6FB
7fB
7�B
7�B
8B
8RB
88B
8lB
8lB
8lB
7�B
8�B
9�B
9�B
9�B
9�B
:^B
;�B
;�B
<PB
<jB
<�B
<�B
=<B
=qB
=qB
=�B
>]B
>�B
>�B
>�B
>�B
?}B
?�B
@OB
@�B
@�B
@�B
AB
A;B
A;B
AoB
AoB
AUB
A�B
BB
BAB
BuB
B[B
B�B
B�B
C�B
C�B
D3B
D3B
DB
D3B
D�B
E9B
E�B
E�B
E�B
E�B
FtB
FtB
FYB
F�B
GEB
G�B
HB
HKB
H�B
HfB
H�B
H�B
IB
I�B
I�B
J#B
J	B
J�B
J�B
KDB
K�B
L0B
LdB
L�B
L�B
L�B
L�B
L�B
MjB
M�B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
O�B
PHB
P�B
P�B
Q B
QB
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
S&B
S&B
S[B
S&B
SuB
S�B
S�B
S�B
T,B
T�B
T�B
T�B
T�B
UB
UMB
UMB
U�B
U�B
U�B
VB
VB
UMB
U�B
V�B
VmB
VSB
VB
U�B
U�B
U�B
V9B
W
B
V�B
W?B
WsB
W?B
W?B
V�B
V�B
W
B
W
B
W�B
XB
W�B
XEB
X�B
Y1B
YeB
Y�B
ZB
ZkB
Y�B
ZQB
ZQB
ZB
ZB
Z�B
Z�B
[�B
\�B
\xB
\]B
\�B
\xB
\�B
]�B
^B
^B
^5B
^5B
^jB
^�B
_;B
_;B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`vB
`�B
a-B
aHB
a|B
a�B
a�B
a�B
a�B
bB
b�B
b�B
b�B
c�B
c�B
c�B
dtB
d�B
eB
eFB
ezB
e�B
e�B
fB
f�B
f�B
gB
gB
gmB
g�B
g�B
h$B
h
B
h>B
h>B
hXB
hsB
h�B
i_B
iDB
iyB
iDB
i�B
i�B
i�B
jKB
j�B
jeB
j�B
j�B
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lB
l�B
l�B
mB
l�B
m)B
m)B
mwB
mCB
m)B
m)B
mwB
mCB
m)B
mCB
mCB
m]B
mwB
mwB
m�B
m�B
nB
n/B
n}B
n�B
n�B
o B
o�B
o�B
o�B
o�B
o�B
p!B
p!B
pUB
p�B
q�B
q�B
rB
q�B
r-B
r�B
shB
s�B
s�B
s�B
tB
tB
s�B
s�B
s�B
t9B
uZB
uB
u?B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
w�B
w�B
xB
xB
xlB
x�B
x�B
y	B
yXB
y�B
y�B
y�B
z*B
z�B
z�B
{0B
{�B
|B
|B
|PB
|�B
|�B
}"B
}<B
}�B
}�B
}qB
~B
~B
~]B
~(B
~�B
~�B
~�B
B
B
}B
�B
� B
�B
�iB
�OB
��B
��B
��B
��B
��B
�B
�oB
�oB
�oB
��B
�AB
��B
��B
�B
�B
�GB
��B
��B
��B
�B
��B
�3B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�9B
�B
�mB
��B
��B
�mB
��B
�mB
�mB
�SB
�mB
�mB
��B
��B
�B
�B
�tB
��B
��B
�Y111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104922  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173929  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173929  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173929                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023936  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023936  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                