CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-11-14T15:36:26Z creation;2018-11-14T15:36:29Z conversion to V3.1;2019-12-18T07:18:56Z update;2022-11-21T05:29:51Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181114153626  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_155                     2C  Dd#!NAVIS_A                         0397                            ARGO 011514                     863 @ؐ�\) 1   @ؐ�З� @<u�oiDg�d#!�.H�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��R@��RA\)A?\)A_\)A\)A��A��A��HA��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BX=qB`=qBg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D��D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�;�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111XA�/A�+A�$�A�+A�&�A�5?A�=qA�=qA�C�A�I�A�I�A�K�A�O�A�M�A�O�A�M�A�Q�A�XA�ZA�ZA�\)A�\)A�^5A�`BA�`BA�`BA�bNA�`BA�\)A�`BA���A���A�XA��A��#A�VA���A��FA�  A��A�Q�A�+A��PA�VA�A�JA��/A��hA���A�ƨA���A�p�A��wA��DA�S�A���A�/A��HA�ffA�VA���A��A��+A�ĜA��-A�S�A�A�A�ƨA�I�A���A��PA�M�A��uA�
=A���A���A���A���A�v�A��A��wA�+A��A�M�A��A���A�XA��A�ZA��RA�x�A�=qA��^A��+A���A�$�A}�A{oAx�uAwx�Av�AuC�At��At��At(�Arv�Aq�wAp��Ao�TAnv�Am"�Al~�Ak�wAj�yAj~�Ai�Af��Ad�Ad�Ad5?AcVAa&�A_�-A^�A]�mA\��A[��AZv�AX��AXbAW�^AWx�AU��AUS�AT�ARE�AO�TAM�#AM�AL�!AKdZAJ�AIx�AHAE�mAEVAD9XAC?}AC33AB�RAA�AA"�A?|�A?A>�9A>5?A=��A=VA<�A<�`A<��A<��A<bA;33A:�A9K�A8z�A7��A7VA5�wA4�uA4M�A4�A3�A2��A1�mA0=qA/�PA.I�A-p�A-%A,jA,bA+�^A+p�A*��A*~�A*~�A*~�A*�+A*�A*r�A*$�A)�A)�A)�A)ƨA)�^A)�7A)�A(�yA(ffA(1A'��A'\)A&�A&��A&I�A%x�A$�!A$9XA"�HA!ƨAVA��A��A��A{AC�AJA��A^5A�
AdZA"�AĜAn�A1AhsA�yA�mA�yA
M�A
bA	�;A	��A	O�A	�A�RAn�A(�At�A&�A��A�!A�DAA�A�;A�hA;dAAbNA �A�A�;A�wA��A�A%A��AffAQ�AJA ��A �+@�=q@�l�@�^5@���@��@��u@���@���@� �@��`@�hs@�@��@�O�@睲@�-@�@�j@�-@��
@�=q@؛�@�&�@ӥ�@��T@�^5@̋D@���@�o@���@ɺ^@�33@�M�@�j@�
=@���@�%@��@�1'@��@��@���@�S�@�"�@���@��@��@���@���@��H@�J@��j@�t�@�+@�n�@���@�X@�9X@��m@���@���@�M�@�@�`B@���@���@�`B@�&�@��j@��@��m@�\)@��H@�M�@��#@���@�Ĝ@��;@���@�+@�-@�z�@��P@��@���@�=q@�@�%@���@��`@���@�Ĝ@��D@���@�dZ@��R@�v�@�{@���@���@�(�@�ƨ@���@�C�@�@���@��@���@��R@��!@�M�@�5?@��@���@��-@��h@�hs@�X@�7L@�&�@��@�+@�V@�@��^@���@�/@��`@���@���@��j@��@� �@�33@�=q@���@��7@�/@�%@��9@��@��D@��@��@���@���@��@�S�@��@���@��y@���@���@�M�@��@���@�X@��`@�(�@�|�@��@��H@��R@�V@���@��7@�`B@�?}@�7L@��@�%@��@��@� �@���@�|�@�t�@�dZ@�C�@�33@�+@�@���@�E�@���@��^@��@�9X@� �@�1@��@�@|�@~��@~V@~{@}�@}O�@}V@|�@|��@|�@|�@{C�@z�!@y��@y%@x1'@v��@v��@v��@v�@vV@u��@up�@t��@t�@tZ@t�@s�m@sƨ@s��@sC�@r�!@q�^@q7L@p��@p��@p�u@p�@pr�@pQ�@p1'@p1'@p �@pb@pb@p  @p  @o�@o�@o�;@o��@o\)@o�@n��@m@l�@lZ@kƨ@k��@kt�@k@jJ@i��@i�#@i�^@i��@i��@ix�@ihs@ihs@ihs@i7L@h�u@g�w@g;d@f�@f5?@e@e`B@d�D@d�@c�
@c�F@cdZ@cC�@c@b�\@b=q@bJ@a�#@a��@a�7@ahs@a7L@a�@`��@`r�@_��@_�@^�+@^@]�h@]`B@]/@\��@\9X@[��@[��@[C�@Z�@Z��@Z�\@Z^5@Z�@Y�#@Y��@Yhs@Y7L@Y7L@Y%@Y%@Y%@X�`@X�`@X��@Xr�@W�w@W;d@W
=@Vȴ@VV@U��@U�-@U��@U�@T��@T9X@S"�@R�@R�H@R�H@R��@R�!@R�\@RM�@Q�^@QG�@Q7L@Q&�@Q%@PĜ@PbN@P  @O�w@O|�@O+@O�@N��@N{@M�-@M`B@L��@L��@L�D@Lz�@Lj@LI�@K�m@Kt�@K@JM�@I��@I��@I�@I��@I�#@I��@I�7@Ix�@Ix�@Ihs@I%@H�u@HbN@HbN@HA�@H  @G�;@G��@G�w@G�@G�P@Gl�@G\)@G\)@G\)@G�@G�@G�@G�@G
=@G
=@G
=@F�y@F��@FE�@E�@EO�@D�j@D�D@Dj@D9X@C�F@C�@CC�@B��@B~�@B=q@A�@A�^@Ahs@@��@@��@@��@@r�@@Q�@@ �@?�@?�@?;d@?+@?
=@>ff@>{@=�T@=��@=p�@=V@<��@<��@<I�@<9X@<1@;S�@:��@:=q@9�@8Ĝ@7�@7�@7|�@7l�@7
=@6�@6�+@6@5@5�@5p�@5p�@5`B@5/@5V@4��@4��@4��@4��@4�D@4I�@41@3��@3@2J@1�^@1x�@0�9@0bN@0 �@/�w@/�P@/l�@/+@.��@.ȴ@.��@.��@.�+@.ff@.@-?}@,��@,�@,�D@,z�@,I�@,9X@,9X@,�@+�
@+��@+��@+�@+dZ@+S�@+33@+o@*��@*��@*~�@*^5@*-@)��@)�^@)�7@)7L@)�@(b@'�@';d@&�y@&��@&��@&��@&��@&E�@%�-@%�h@%p�@%?}@%�@$�/@$j@$9X@$1@#�
@#�F@#��@#��@#dZ@"�@"�\@"�\@"n�@"M�@"J@!�@!��@!��@!�^@!�7@!hs@!&�@!%@ �9@ 1'@l�@+@��@V@5?@$�@{@�@@�h@O�@/@��@�D@9X@�m@��@��@dZ@@=q@��@G�@%@��@Q�@1'@�w@�w@�w@�@�@��@��@�P@|�@l�@l�@;d@��@��@�@�@�@p�@O�@��@�@�@��@�D@I�@1@��@�F@�@dZ@C�@�@��@��@��@��@�!@��@M�@��@X@�9@A�@ �@  @��@|�@;d@�y@�R@��@v�@ff@�@�-@�@O�@O�@?}@?}@��@��@�j@�@�@��@z�@z�@j@j@j@Z@Z@9X@1@��@��@��@��@��@�m@�
@�F@��@��@��@��@dZ@C�@33@o@@
�@
��@
~�@
n�@
^5@
-@
�@
J@	x�@	7L@	&�@	&�@	&�@	�@��@�9@�u@�@Q�@A�@1'@ �@b@b@�w@
=@�y@�@�@�@�@�R@��@v�@ff@V@�@p�@O�@/@V@�@j@j@I�@(�@�
@��@��@�@�@t�@dZ@C�@@�@�H@��@�\@-@�@J@��@��@��@��@��@�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111XA�/A�+A�$�A�+A�&�A�5?A�=qA�=qA�C�A�I�A�I�A�K�A�O�A�M�A�O�A�M�A�Q�A�XA�ZA�ZA�\)A�\)A�^5A�`BA�`BA�`BA�bNA�`BA�\)A�`BA���A���A�XA��A��#A�VA���A��FA�  A��A�Q�A�+A��PA�VA�A�JA��/A��hA���A�ƨA���A�p�A��wA��DA�S�A���A�/A��HA�ffA�VA���A��A��+A�ĜA��-A�S�A�A�A�ƨA�I�A���A��PA�M�A��uA�
=A���A���A���A���A�v�A��A��wA�+A��A�M�A��A���A�XA��A�ZA��RA�x�A�=qA��^A��+A���A�$�A}�A{oAx�uAwx�Av�AuC�At��At��At(�Arv�Aq�wAp��Ao�TAnv�Am"�Al~�Ak�wAj�yAj~�Ai�Af��Ad�Ad�Ad5?AcVAa&�A_�-A^�A]�mA\��A[��AZv�AX��AXbAW�^AWx�AU��AUS�AT�ARE�AO�TAM�#AM�AL�!AKdZAJ�AIx�AHAE�mAEVAD9XAC?}AC33AB�RAA�AA"�A?|�A?A>�9A>5?A=��A=VA<�A<�`A<��A<��A<bA;33A:�A9K�A8z�A7��A7VA5�wA4�uA4M�A4�A3�A2��A1�mA0=qA/�PA.I�A-p�A-%A,jA,bA+�^A+p�A*��A*~�A*~�A*~�A*�+A*�A*r�A*$�A)�A)�A)�A)ƨA)�^A)�7A)�A(�yA(ffA(1A'��A'\)A&�A&��A&I�A%x�A$�!A$9XA"�HA!ƨAVA��A��A��A{AC�AJA��A^5A�
AdZA"�AĜAn�A1AhsA�yA�mA�yA
M�A
bA	�;A	��A	O�A	�A�RAn�A(�At�A&�A��A�!A�DAA�A�;A�hA;dAAbNA �A�A�;A�wA��A�A%A��AffAQ�AJA ��A �+@�=q@�l�@�^5@���@��@��u@���@���@� �@��`@�hs@�@��@�O�@睲@�-@�@�j@�-@��
@�=q@؛�@�&�@ӥ�@��T@�^5@̋D@���@�o@���@ɺ^@�33@�M�@�j@�
=@���@�%@��@�1'@��@��@���@�S�@�"�@���@��@��@���@���@��H@�J@��j@�t�@�+@�n�@���@�X@�9X@��m@���@���@�M�@�@�`B@���@���@�`B@�&�@��j@��@��m@�\)@��H@�M�@��#@���@�Ĝ@��;@���@�+@�-@�z�@��P@��@���@�=q@�@�%@���@��`@���@�Ĝ@��D@���@�dZ@��R@�v�@�{@���@���@�(�@�ƨ@���@�C�@�@���@��@���@��R@��!@�M�@�5?@��@���@��-@��h@�hs@�X@�7L@�&�@��@�+@�V@�@��^@���@�/@��`@���@���@��j@��@� �@�33@�=q@���@��7@�/@�%@��9@��@��D@��@��@���@���@��@�S�@��@���@��y@���@���@�M�@��@���@�X@��`@�(�@�|�@��@��H@��R@�V@���@��7@�`B@�?}@�7L@��@�%@��@��@� �@���@�|�@�t�@�dZ@�C�@�33@�+@�@���@�E�@���@��^@��@�9X@� �@�1@��@�@|�@~��@~V@~{@}�@}O�@}V@|�@|��@|�@|�@{C�@z�!@y��@y%@x1'@v��@v��@v��@v�@vV@u��@up�@t��@t�@tZ@t�@s�m@sƨ@s��@sC�@r�!@q�^@q7L@p��@p��@p�u@p�@pr�@pQ�@p1'@p1'@p �@pb@pb@p  @p  @o�@o�@o�;@o��@o\)@o�@n��@m@l�@lZ@kƨ@k��@kt�@k@jJ@i��@i�#@i�^@i��@i��@ix�@ihs@ihs@ihs@i7L@h�u@g�w@g;d@f�@f5?@e@e`B@d�D@d�@c�
@c�F@cdZ@cC�@c@b�\@b=q@bJ@a�#@a��@a�7@ahs@a7L@a�@`��@`r�@_��@_�@^�+@^@]�h@]`B@]/@\��@\9X@[��@[��@[C�@Z�@Z��@Z�\@Z^5@Z�@Y�#@Y��@Yhs@Y7L@Y7L@Y%@Y%@Y%@X�`@X�`@X��@Xr�@W�w@W;d@W
=@Vȴ@VV@U��@U�-@U��@U�@T��@T9X@S"�@R�@R�H@R�H@R��@R�!@R�\@RM�@Q�^@QG�@Q7L@Q&�@Q%@PĜ@PbN@P  @O�w@O|�@O+@O�@N��@N{@M�-@M`B@L��@L��@L�D@Lz�@Lj@LI�@K�m@Kt�@K@JM�@I��@I��@I�@I��@I�#@I��@I�7@Ix�@Ix�@Ihs@I%@H�u@HbN@HbN@HA�@H  @G�;@G��@G�w@G�@G�P@Gl�@G\)@G\)@G\)@G�@G�@G�@G�@G
=@G
=@G
=@F�y@F��@FE�@E�@EO�@D�j@D�D@Dj@D9X@C�F@C�@CC�@B��@B~�@B=q@A�@A�^@Ahs@@��@@��@@��@@r�@@Q�@@ �@?�@?�@?;d@?+@?
=@>ff@>{@=�T@=��@=p�@=V@<��@<��@<I�@<9X@<1@;S�@:��@:=q@9�@8Ĝ@7�@7�@7|�@7l�@7
=@6�@6�+@6@5@5�@5p�@5p�@5`B@5/@5V@4��@4��@4��@4��@4�D@4I�@41@3��@3@2J@1�^@1x�@0�9@0bN@0 �@/�w@/�P@/l�@/+@.��@.ȴ@.��@.��@.�+@.ff@.@-?}@,��@,�@,�D@,z�@,I�@,9X@,9X@,�@+�
@+��@+��@+�@+dZ@+S�@+33@+o@*��@*��@*~�@*^5@*-@)��@)�^@)�7@)7L@)�@(b@'�@';d@&�y@&��@&��@&��@&��@&E�@%�-@%�h@%p�@%?}@%�@$�/@$j@$9X@$1@#�
@#�F@#��@#��@#dZ@"�@"�\@"�\@"n�@"M�@"J@!�@!��@!��@!�^@!�7@!hs@!&�@!%@ �9@ 1'@l�@+@��@V@5?@$�@{@�@@�h@O�@/@��@�D@9X@�m@��@��@dZ@@=q@��@G�@%@��@Q�@1'@�w@�w@�w@�@�@��@��@�P@|�@l�@l�@;d@��@��@�@�@�@p�@O�@��@�@�@��@�D@I�@1@��@�F@�@dZ@C�@�@��@��@��@��@�!@��@M�@��@X@�9@A�@ �@  @��@|�@;d@�y@�R@��@v�@ff@�@�-@�@O�@O�@?}@?}@��@��@�j@�@�@��@z�@z�@j@j@j@Z@Z@9X@1@��@��@��@��@��@�m@�
@�F@��@��@��@��@dZ@C�@33@o@@
�@
��@
~�@
n�@
^5@
-@
�@
J@	x�@	7L@	&�@	&�@	&�@	�@��@�9@�u@�@Q�@A�@1'@ �@b@b@�w@
=@�y@�@�@�@�@�R@��@v�@ff@V@�@p�@O�@/@V@�@j@j@I�@(�@�
@��@��@�@�@t�@dZ@C�@@�@�H@��@�\@-@�@J@��@��@��@��@��@�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�JB�7B�7B�=B�=B�=B�=B�7B�=B�=B�7B�7B�7B�7B�7B�=B�=B�=B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�1B�B�B�7B�JB�\B�\B�oB�VB�+B�=B�%B�Bz�Bs�BffB`BB[#BO�B>wB6FB1'B(�B"�B�BPB  B��B�B�B�yB�`B�NB�BB�/B��BŢB�3B��B��B�{B�Bz�Bv�Bk�BaHBN�B49B,B'�B �B�BuB1B
��B
�B
�yB
�`B
�BB
�)B
��B
ȴB
ĜB
�}B
�B
��B
��B
�bB
|�B
m�B
]/B
W
B
Q�B
H�B
E�B
C�B
?}B
5?B
/B
&�B
�B
�B
VB

=B
B	��B	��B	�B	�;B	��B	��B	��B	ŢB	�LB	�B	��B	��B	��B	�uB	�JB	�B	�B	~�B	{�B	{�B	� B	w�B	p�B	bNB	P�B	R�B	S�B	K�B	E�B	7LB	.B	!�B	�B	�B	uB	uB	�B	oB	PB	B��B��B��B��B��B��B��B�B�B�B�B�fB�HB�5B�B�B��B��BɺBǮBĜB��B�qB�XB�FB�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�=B�B~�Bz�Bx�Bw�Bu�Br�Bn�BhsBdZBbNBaHB`BB_;B^5B\)BZBW
BP�BH�BC�BB�BB�BA�BA�B@�B?}B>wB=qB=qB<jB;dB;dB:^B:^B9XB8RB8RB7LB7LB6FB5?B5?B5?B5?B49B33B2-B2-B1'B0!B.B,B)�B(�B'�B&�B%�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBbBhBhBhBhBhBhBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B"�B"�B(�B)�B)�B,B,B,B-B.B.B33B5?B5?B6FB6FB8RB9XB:^B<jB=qB>wB@�BB�BC�BE�BG�BL�BN�BP�BQ�BR�BS�BW
BXBXBXBXBYB[#B^5BbNBcTBe`BgmBm�Bp�Bt�Bv�Bx�By�By�Bz�Bz�Bz�Bz�B|�B}�B}�B~�B�B�B�B�B�%B�+B�bB��B��B��B��B��B��B��B��B��B��B��B�B�LB��BĜBɺB��B��B��B��B��B�B�B�#B�/B�5B�BB�HB�TB�TB�ZB�`B�mB�sB�B�B�B��B��B��B��B��B	B	B	+B		7B	
=B	
=B	DB	DB	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	-B	/B	1'B	2-B	33B	5?B	8RB	9XB	;dB	<jB	>wB	>wB	?}B	@�B	@�B	C�B	G�B	I�B	K�B	M�B	Q�B	XB	YB	ZB	\)B	`BB	dZB	e`B	ffB	hsB	iyB	jB	k�B	l�B	l�B	m�B	o�B	s�B	v�B	x�B	y�B	z�B	{�B	{�B	|�B	|�B	}�B	}�B	}�B	~�B	~�B	~�B	~�B	� B	� B	�B	�B	�B	�B	�1B	�DB	�VB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�jB	�wB	��B	B	ÖB	ĜB	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�/B	�;B	�HB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B

=B
DB
JB
JB
JB
VB
VB
VB
\B
bB
bB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
.B
/B
0!B
0!B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
<jB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
R�B
S�B
S�B
T�B
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�JB�7B�7B�=B�=B�#B�#B�7B�#B�=B�7B�7B�7B�7B�7B�=B�=B�=B�7B�7B�7B�7B�7B�7B�7B�7B�7B�lB��B��B��B��B��B��B�@B�[B��B�oB��B�dB�KB��B~wBv�Bh�Bb�B^jBTFB@�B88B2�B*B$�BIB�B;B�qB�B�B�KB��B��B�-B�;B֡BȚB��B�2B�OB��B�B{�Bx�BmwBd�BR�B5�B,�B(�B!�B�BB
�B
�8B
�iB
�KB
�2B
�-B
��B
�@B
ɠB
�B
��B
��B
��B
�B
��B
}B
p!B
^�B
XB
S�B
I7B
F?B
D�B
A;B
6FB
0UB
(>B
!bB
�B
BB
DB
%B	��B	��B	��B	��B	өB	��B	ϑB	��B	�	B	�wB	��B	�4B	�#B	�2B	�B	�B	��B	�B	}�B	|�B	��B	z^B	shB	dtB	Q�B	S�B	UgB	L�B	G�B	9XB	0UB	#B	�B	�B	�B	FB	�B	�B	B	�B��B��B��B�rB�B��B�B�9B�B��B�B��B�hB�;B�WB��B�B�DB�=BȴB��B��B�cB��B��B�3B��B��B��B��B�B��B�RB�B��B��B��B�2B�FB�,B��B��B�@B�B�:B�hB�HB��B�OB�/B�CB�=B�1B�EB��B��B�oB�bB�dB�1B��B{�ByrBx�Bw2Bt�Bq'BjKBe,Bb�Ba�B`�B_�B^�B]IB[�BY�BVmBK^BD3BCBB�BBBA�BA B@ B?B>]B=�B<�B;�B;�B:�B:�B9�B8�B8�B8B7�B6�B5tB5�B5�B5�B4�B3�B2�B2�B1�B1�B/5B.B+�B)�B(�B'mB&�B&LB$&B"�B!�B�B]B�B�B�B�B+BsB$B$B�B�B�B�B�BoBoBB�B�BoB�BaB�B�BSB?B�B�B�B�B�B�BBBeB#B#BqBdB~B�B �B#:B#�B$B%FB)�B*eB*�B,=B,WB,qB-�B/ B/iB3�B5�B5�B6�B6�B8�B9�B:�B<�B=�B?.BA BB�BD3BF�BH�BMjBOvBQ4BRoBS[BT�BW$BX+BX+BX_BXyBY�B[�B^�Bb�Bc�Be�Bh$Bm�Bp�BuBwBy	By�BzB{Bz�B{0B{0B}"B~(B~(BHB�;B�[B�-B�GB�tB��B�NB�)B�B��B��B�@B�>B�$B�$B�$B�$B��B��B��B��B��B�#B��B�B�B�B�:B�9B�KB�WB�~BބB�vB�|B�nB�B�B�B�B��B��B��B�AB�`B�0B�(B�(B�cB	uB	SB	_B		RB	
XB	
rB	^B	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	B	 'B	"B	$tB	)_B	-)B	/5B	1AB	2GB	3hB	5�B	8lB	9�B	;�B	<�B	>�B	>�B	?�B	@�B	@�B	C�B	G�B	J=B	LB	N<B	RTB	XB	Y1B	Z7B	\xB	`vB	dtB	e�B	f�B	h�B	i�B	j�B	k�B	l�B	l�B	m�B	o�B	s�B	v�B	x�B	zB	z�B	|B	|B	}"B	}"B	~B	~B	~B	B	~�B	B	B	�B	�B	�;B	�AB	�aB	�mB	��B	�xB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�8B	�DB	�WB	�/B	�5B	�5B	�;B	�AB	�aB	�MB	�TB	�ZB	�ZB	�`B	�fB	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	� B	�B	�B	�B	��B	�B	�&B	�B	�B	�FB	�SB	�EB	�1B	�kB	�WB	�IB	�IB	�IB	�dB	ߊB	�B	�tB	�`B	�fB	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�<B	�B
 B
  B
 B
  B
 B
B
 B
B
;B
;B
AB
GB
B
-B
3B
MB
B
9B
9B
9B
SB
%B
%B
%B
?B
?B
%B
%B
+B
EB
+B
EB
_B
KB
�B
	�B

�B
^B
~B
~B
�B
�B
�B
�B
vB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 �B
!-B
$&B
%B
%�B
%�B
%�B
'B
'B
($B
)B
)B
)�B
)�B
*B
*B
*B
*0B
*B
+6B
+B
+B
+6B
,"B
,WB
,WB
.}B
/5B
0UB
0oB
2GB
2aB
3MB
3MB
4TB
4TB
4TB
4TB
5tB
5ZB
5ZB
5tB
5tB
5�B
7fB
8lB
8lB
8lB
8�B
9XB
9rB
9rB
9�B
9rB
:^B
:^B
:xB
:xB
:�B
:xB
:xB
;B
;B
;�B
;B
;�B
;�B
<�B
<�B
=�B
<�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
KB
L�B
MB
M�B
NB
N�B
N�B
OB
N�B
N�B
N�B
N�B
OB
O(B
O�B
O�B
O�B
O�B
O�B
QB
Q4B
R:B
S&B
T,B
T,B
UB
VB
V9B
W
B
W
B
W
B
W
B
W
B
W
B
W$B
W
B
W$B
W
B
W$B
W?B
W�B
Y1B
YB
YB
Y1B
YKB
ZQB
Z7B
ZB
[#B
[WB
[=B
[=B
[WB
\CB
\]B
\]B
\CB
]IB
]IB
]IB
]/B
]/B
]IB
]IB
]dB
^jB
^�B
_pB
`\B
`\B
abB
abB
a|B
b�B
bhB
b�B
c�B
cnB
cnB
c�B
dtB
dtB
dtB
e`B
ezB
e`B
ezB
ezB
f�B
ffB
ffB
f�B
f�B
f�B
ffB
gmB
g�B
g�B
gmB
g�B
g�B
gmB
h�B
h�B
hsB
hsB
hsB
hsB
h�B
h�B
hsB
hsB
hsB
h�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811250037302018112500373020181125003730202211182136582022111821365820221118213658201811260019592018112600195920181126001959  JA  ARFMdecpA19c                                                                20181115003625  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181114153626  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181114153627  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181114153628  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181114153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181114153628  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181114153628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181114153628  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181114153629  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181114153629                      G�O�G�O�G�O�                JA  ARUP                                                                        20181114155507                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181114153654  CV  JULD            G�O�G�O�Fą                JM  ARCAJMQC2.0                                                                 20181124153730  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181124153730  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181125151959  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                