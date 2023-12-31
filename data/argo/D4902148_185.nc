CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-10T18:37:57Z creation;2019-09-10T18:38:01Z conversion to V3.1;2022-11-21T05:28:22Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Md   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pD   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20190910183757  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_185                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�ۣB^Ѐ1   @�ۤ�΀@<u��O��dl�Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA�z�A�z�A��HB�
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
�qD}qD�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�
D}qD�qD}qD�qD}qD�qD��D�D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D뾸D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��D�1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��yA��yA��A��A�~�A�A���Aۺ^A��Aش9A�JA�C�A���A�|�A�bA�z�A���A��;A��A��A�p�A�M�A���A���A�1A�+A�=qA�S�A��A�/A�(�A�`BA���A���A�{A��A���A�
=A��;A��^A��PA�ffA�9XA���A��!A�(�A� �A���A�;dA��A��!A�p�A�5?A��!A�ȴA��A���A�-A�bNA�=qA�JA��`A�`BA���A��+A�ȴA�t�A���A��yA�ĜA���A�z�A�`BA�9XA�
=A��-A�bA���A���A���A�n�A�=qA�ffA���A�7LA��RA�XA��A�C�A��9A�\)A�  A�z�A��!A�Q�A��TA��7A���A�l�A�1'A�ȴA��yA�dZA��A�bNA���A���A���A�+A?}A}\)A|v�A|bA{�A{�Az  Ay
=Aw�Av�HAut�AtI�As�hArjAq�Ap�Ao��Anr�Am�^Al�AlE�AlAi7LAfjAd��Abn�AaS�A`n�A^��A\��A[ƨAYAX�AXE�AWt�AU�#ATv�AS�7AR��AR�AQ�AQ`BAO�FAO�PAOC�AN^5AM�FAK�AJĜAJE�AI��AH��AHAG�;AGG�AE�#AD��AC��AB��AB1'AA��AA"�A@�A@�!A@z�A@  A?hsA?K�A?;dA?A>r�A>-A=�hA=oA;�hA:��A:I�A9��A9
=A8^5A7G�A6��A5�;A5\)A4ZA1��A0ĜA0��A0A�A/�
A.�A-S�A,��A+/A)��A(��A'��A'&�A&1'A%�^A%XA$��A#�^A#A"�9A"bNA!�wA!`BA!&�A ��A 1A�wAt�A��Ax�Az�AbAAJA�PA�HA�mAdZA�`AG�A�A��A�PA;dA�\A��A33A9XAK�A�A�A-A	�
A	|�A	dZA	%A�A�/A  A�PA{A�wA��AS�A �9@��P@�V@�O�@�Z@��@��`@�I�@�;d@�@��@�r�@�C�@@��^@�V@��;@���@�p�@�1@�\)@�R@���@��m@�@��@��
@�C�@�E�@݉7@�A�@��T@�1'@���@�b@�{@�1'@϶F@�;d@�o@��@�ȴ@Ώ\@��@��#@���@�"�@�X@�b@���@��@��@ă@�33@��@�(�@�;d@��+@�$�@�{@���@���@�X@�Z@���@��@��@��@��!@��@���@���@���@��@�+@�ȴ@�~�@�5?@���@�7L@�Ĝ@��@���@�l�@�33@���@�@���@�t�@���@�V@��@�7L@�%@��@�dZ@��@���@�O�@�V@�Z@�C�@�
=@��H@�ȴ@��!@��+@�^5@��@�&�@�j@�  @�t�@���@���@���@��@�G�@��j@� �@��!@��-@�%@�A�@��@���@�S�@�+@��\@�V@�@��-@�X@��@�%@���@�Ĝ@�bN@��@�C�@���@���@�M�@��h@�O�@���@�A�@���@�\)@�"�@��@��@��-@�p�@���@�9X@��@��F@��@�C�@�
=@�=q@���@��h@��@��@��9@��@��@���@�A�@��;@��@��@�|�@�;d@��@���@��+@�$�@���@�x�@�O�@���@��@��F@�S�@��@��y@���@�M�@�{@�@�{@�@��@���@�@�`B@��`@��j@�Ĝ@���@���@��/@���@���@�Ĝ@���@��D@�Q�@�@
=@~�+@}��@}�@|��@|��@|�@|1@{ƨ@{S�@{@z~�@z=q@zJ@y�^@yX@yG�@yX@x�`@x �@w�@w�w@w�@v@up�@t��@t��@t(�@st�@s33@r�H@qx�@p��@pA�@p1'@o��@o�@m�T@m�h@m�@m�h@m/@l��@l��@l9X@k�
@kC�@j��@j��@j�\@j^5@j-@i��@i�^@i��@i�7@ihs@iX@i7L@i�@h�u@hQ�@h1'@h1'@h �@g|�@g�@f�R@fV@e�@d��@dz�@dI�@d(�@d1@c��@c�m@cƨ@c��@c�@co@b��@b��@b��@bn�@bM�@b=q@b�@a��@a�@a�@a��@a�7@ahs@ahs@aG�@`��@`��@`r�@`1'@`bN@_�@^�R@^V@^{@]��@]�h@]p�@]?}@\�D@[��@[dZ@[S�@[33@[o@[o@["�@Z�H@Z~�@Y�^@Y��@Y�7@Yx�@Y��@Y�^@Y��@Y��@Y��@Y&�@X�u@Xr�@XA�@Xb@W�w@Wl�@Vȴ@V{@U�-@UV@T(�@R�@R-@Q�@Q�7@Q&�@P��@PA�@P  @O�w@O|�@O\)@O
=@N�R@N��@N5?@M�h@MO�@L��@L�@Lz�@L9X@K�m@Kt�@KC�@K"�@J�!@I��@I�#@I��@I��@H��@H�@G�w@Gl�@G+@G+@G+@G;d@GK�@G\)@Gl�@G|�@G\)@G\)@GK�@F�y@F5?@E�-@E`B@EO�@E?}@D�@D�@D�D@DZ@D(�@C��@Cƨ@C�F@C��@CdZ@Co@B��@B~�@Bn�@BM�@B�@A�@A��@A��@A�^@A��@A��@AG�@@�u@@r�@@bN@@A�@?�;@?�P@?�@>ȴ@=�@=�@<��@<�j@<z�@<(�@;ƨ@;�@;dZ@;dZ@;dZ@;dZ@;S�@;S�@;"�@:��@:��@:=q@9��@9�7@97L@8��@8Ĝ@8��@8�u@7�@7�w@7K�@6$�@4��@4�j@4�@4�D@3�m@3�@3S�@2�@2��@2n�@2M�@2M�@2=q@2=q@2-@2-@2�@2J@2J@1��@1��@1�@1�@1�#@1�^@1��@1x�@0Ĝ@/|�@/
=@.ȴ@.��@.E�@-@-O�@-?}@-V@,�@,��@,I�@,1@+dZ@+C�@+33@+"�@+o@+o@+@*�H@*�\@*M�@)��@)�#@)hs@)&�@(��@(A�@(  @'��@'�P@'l�@'\)@'K�@';d@'�@&�y@&�@&�R@&��@&��@&v�@&E�@%�@%O�@$�@$Z@$Z@$I�@$9X@$�@$�@$1@$1@#�F@#S�@#33@#33@#o@"�H@"��@"��@"�!@"�@!�#@!x�@!�@!%@ �`@ �`@ ��@ Ĝ@ ��@ r�@ bN@ A�@ 1'@ b@�;@�P@l�@l�@\)@K�@+@�@��@�y@�R@ff@@�T@�-@`B@V@�/@��@�j@�@�D@j@�@ƨ@��@��@��@S�@�@��@��@~�@J@�^@��@��@��@��@�7@G�@%@�`@��@�u@ �@�;@�w@�P@+@�y@��@V@@�-@�@?}@V@�/@�D@z�@(�@1@ƨ@��@33@�!@M�@J@��@X@&�@�u@bN@ �@ �@�@��@��@�P@\)@��@�R@v�@V@$�@$�@@��@�-@p�@O�@/@/@?}@V@��@�D@Z@I�@9X@(�@�@�@1@1@��@��@��@ƨ@t�@C�@33@
�H@
��@
�!@
��@
�\@
�\@
^5@	�@	��@	�7@	hs@	X@	X@	G�@	7L@	7L@	%@�`@��@Q�@  @��@�w@�w@l�@
=@ȴ@�+@V@5?@{@@O�@?}@��@�j@��@�D@j@I�@9X@�@1@�
@�F@ƨ@�F@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��yA��yA��A��A�~�A�A���Aۺ^A��Aش9A�JA�C�A���A�|�A�bA�z�A���A��;A��A��A�p�A�M�A���A���A�1A�+A�=qA�S�A��A�/A�(�A�`BA���A���A�{A��A���A�
=A��;A��^A��PA�ffA�9XA���A��!A�(�A� �A���A�;dA��A��!A�p�A�5?A��!A�ȴA��A���A�-A�bNA�=qA�JA��`A�`BA���A��+A�ȴA�t�A���A��yA�ĜA���A�z�A�`BA�9XA�
=A��-A�bA���A���A���A�n�A�=qA�ffA���A�7LA��RA�XA��A�C�A��9A�\)A�  A�z�A��!A�Q�A��TA��7A���A�l�A�1'A�ȴA��yA�dZA��A�bNA���A���A���A�+A?}A}\)A|v�A|bA{�A{�Az  Ay
=Aw�Av�HAut�AtI�As�hArjAq�Ap�Ao��Anr�Am�^Al�AlE�AlAi7LAfjAd��Abn�AaS�A`n�A^��A\��A[ƨAYAX�AXE�AWt�AU�#ATv�AS�7AR��AR�AQ�AQ`BAO�FAO�PAOC�AN^5AM�FAK�AJĜAJE�AI��AH��AHAG�;AGG�AE�#AD��AC��AB��AB1'AA��AA"�A@�A@�!A@z�A@  A?hsA?K�A?;dA?A>r�A>-A=�hA=oA;�hA:��A:I�A9��A9
=A8^5A7G�A6��A5�;A5\)A4ZA1��A0ĜA0��A0A�A/�
A.�A-S�A,��A+/A)��A(��A'��A'&�A&1'A%�^A%XA$��A#�^A#A"�9A"bNA!�wA!`BA!&�A ��A 1A�wAt�A��Ax�Az�AbAAJA�PA�HA�mAdZA�`AG�A�A��A�PA;dA�\A��A33A9XAK�A�A�A-A	�
A	|�A	dZA	%A�A�/A  A�PA{A�wA��AS�A �9@��P@�V@�O�@�Z@��@��`@�I�@�;d@�@��@�r�@�C�@@��^@�V@��;@���@�p�@�1@�\)@�R@���@��m@�@��@��
@�C�@�E�@݉7@�A�@��T@�1'@���@�b@�{@�1'@϶F@�;d@�o@��@�ȴ@Ώ\@��@��#@���@�"�@�X@�b@���@��@��@ă@�33@��@�(�@�;d@��+@�$�@�{@���@���@�X@�Z@���@��@��@��@��!@��@���@���@���@��@�+@�ȴ@�~�@�5?@���@�7L@�Ĝ@��@���@�l�@�33@���@�@���@�t�@���@�V@��@�7L@�%@��@�dZ@��@���@�O�@�V@�Z@�C�@�
=@��H@�ȴ@��!@��+@�^5@��@�&�@�j@�  @�t�@���@���@���@��@�G�@��j@� �@��!@��-@�%@�A�@��@���@�S�@�+@��\@�V@�@��-@�X@��@�%@���@�Ĝ@�bN@��@�C�@���@���@�M�@��h@�O�@���@�A�@���@�\)@�"�@��@��@��-@�p�@���@�9X@��@��F@��@�C�@�
=@�=q@���@��h@��@��@��9@��@��@���@�A�@��;@��@��@�|�@�;d@��@���@��+@�$�@���@�x�@�O�@���@��@��F@�S�@��@��y@���@�M�@�{@�@�{@�@��@���@�@�`B@��`@��j@�Ĝ@���@���@��/@���@���@�Ĝ@���@��D@�Q�@�@
=@~�+@}��@}�@|��@|��@|�@|1@{ƨ@{S�@{@z~�@z=q@zJ@y�^@yX@yG�@yX@x�`@x �@w�@w�w@w�@v@up�@t��@t��@t(�@st�@s33@r�H@qx�@p��@pA�@p1'@o��@o�@m�T@m�h@m�@m�h@m/@l��@l��@l9X@k�
@kC�@j��@j��@j�\@j^5@j-@i��@i�^@i��@i�7@ihs@iX@i7L@i�@h�u@hQ�@h1'@h1'@h �@g|�@g�@f�R@fV@e�@d��@dz�@dI�@d(�@d1@c��@c�m@cƨ@c��@c�@co@b��@b��@b��@bn�@bM�@b=q@b�@a��@a�@a�@a��@a�7@ahs@ahs@aG�@`��@`��@`r�@`1'@`bN@_�@^�R@^V@^{@]��@]�h@]p�@]?}@\�D@[��@[dZ@[S�@[33@[o@[o@["�@Z�H@Z~�@Y�^@Y��@Y�7@Yx�@Y��@Y�^@Y��@Y��@Y��@Y&�@X�u@Xr�@XA�@Xb@W�w@Wl�@Vȴ@V{@U�-@UV@T(�@R�@R-@Q�@Q�7@Q&�@P��@PA�@P  @O�w@O|�@O\)@O
=@N�R@N��@N5?@M�h@MO�@L��@L�@Lz�@L9X@K�m@Kt�@KC�@K"�@J�!@I��@I�#@I��@I��@H��@H�@G�w@Gl�@G+@G+@G+@G;d@GK�@G\)@Gl�@G|�@G\)@G\)@GK�@F�y@F5?@E�-@E`B@EO�@E?}@D�@D�@D�D@DZ@D(�@C��@Cƨ@C�F@C��@CdZ@Co@B��@B~�@Bn�@BM�@B�@A�@A��@A��@A�^@A��@A��@AG�@@�u@@r�@@bN@@A�@?�;@?�P@?�@>ȴ@=�@=�@<��@<�j@<z�@<(�@;ƨ@;�@;dZ@;dZ@;dZ@;dZ@;S�@;S�@;"�@:��@:��@:=q@9��@9�7@97L@8��@8Ĝ@8��@8�u@7�@7�w@7K�@6$�@4��@4�j@4�@4�D@3�m@3�@3S�@2�@2��@2n�@2M�@2M�@2=q@2=q@2-@2-@2�@2J@2J@1��@1��@1�@1�@1�#@1�^@1��@1x�@0Ĝ@/|�@/
=@.ȴ@.��@.E�@-@-O�@-?}@-V@,�@,��@,I�@,1@+dZ@+C�@+33@+"�@+o@+o@+@*�H@*�\@*M�@)��@)�#@)hs@)&�@(��@(A�@(  @'��@'�P@'l�@'\)@'K�@';d@'�@&�y@&�@&�R@&��@&��@&v�@&E�@%�@%O�@$�@$Z@$Z@$I�@$9X@$�@$�@$1@$1@#�F@#S�@#33@#33@#o@"�H@"��@"��@"�!@"�@!�#@!x�@!�@!%@ �`@ �`@ ��@ Ĝ@ ��@ r�@ bN@ A�@ 1'@ b@�;@�P@l�@l�@\)@K�@+@�@��@�y@�R@ff@@�T@�-@`B@V@�/@��@�j@�@�D@j@�@ƨ@��@��@��@S�@�@��@��@~�@J@�^@��@��@��@��@�7@G�@%@�`@��@�u@ �@�;@�w@�P@+@�y@��@V@@�-@�@?}@V@�/@�D@z�@(�@1@ƨ@��@33@�!@M�@J@��@X@&�@�u@bN@ �@ �@�@��@��@�P@\)@��@�R@v�@V@$�@$�@@��@�-@p�@O�@/@/@?}@V@��@�D@Z@I�@9X@(�@�@�@1@1@��@��@��@ƨ@t�@C�@33@
�H@
��@
�!@
��@
�\@
�\@
^5@	�@	��@	�7@	hs@	X@	X@	G�@	7L@	7L@	%@�`@��@Q�@  @��@�w@�w@l�@
=@ȴ@�+@V@5?@{@@O�@?}@��@�j@��@�D@j@I�@9X@�@1@�
@�F@ƨ@�F@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B�B  BB%BPBB��B�B�5B�)B�5B�)B�#B�#B�B�B��B��B��BȴBĜB�}B�jB�?B�B��B��B��B��B��B�oB�PB�DB�7B�1B�1B�+B�%B�B�B�B~�By�Bw�Bw�By�Bo�Bm�Bm�BiyB\)BJ�B?}B49B,B-B)�B(�B�B{BbBB��B��B��B�B�B�B�B�sB�TB�#B��BȴB�XB�9B�'B�B��B�\B�+B~�Bw�Br�Be`BZBT�BM�BB�B33B)�B!�B�B	7B
�B
�B
�`B
��B
��B
ÖB
�XB
�B
��B
��B
�uB
�=B
{�B
s�B
p�B
m�B
hsB
`BB
XB
O�B
F�B
;dB
1'B
+B
"�B
�B
�B
oB
JB

=B
B
  B	��B	�B	�/B	��B	B	�dB	�9B	��B	��B	��B	�VB	�DB	�+B	�B	z�B	s�B	m�B	iyB	ffB	dZB	`BB	YB	XB	XB	R�B	N�B	E�B	@�B	=qB	;dB	8RB	49B	2-B	0!B	0!B	,B	%�B	!�B	�B	�B	�B	�B	�B	�B	�B	oB	hB	hB	\B	JB	
=B	+B	B��B��B��B�B�B�B�mB�HB�#B�
B��BŢB�}BĜBŢBÖB��B�XB�?B�B��B��B��B��B��B��B��B��B��B�oB�hB�bB�VB�JB�DB�1B�+B�%B�B�Bz�Bx�By�Bx�Bw�Bu�Bs�Bq�Bp�Bm�BgmBffBcTBbNB`BB_;B_;B]/BZBXBT�BP�BM�BI�BH�BH�BF�BC�B?}B=qB9XB33B2-B49B9XB6FB49B2-B0!B/B-B,B+B)�B'�B'�B'�B'�B&�B$�B#�B#�B#�B%�B'�B&�B&�B&�B%�B&�B%�B%�B%�B%�B%�B$�B$�B#�B!�B!�B$�B&�B'�B(�B)�B)�B)�B)�B)�B)�B+B-B0!B1'B33B49B6FB6FB6FB8RB:^B;dB<jB<jB<jB=qB=qB=qB?}BA�BB�BB�BD�BH�BJ�BK�BL�BL�BN�BP�BQ�BQ�BQ�BR�BR�BS�BW
BYBZB[#B\)B^5B`BBbNBcTBcTBcTBdZBdZBffBgmBjBl�Bn�Bn�Bo�Bp�Bq�Bq�Bq�Bq�Br�Br�Bt�Bx�B{�B|�B~�B� B�B�B�B�B�B�%B�JB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�9B�dB�wB�}B��BƨBǮB��B��B��B��B��B��B�#B�5B�;B�TB�mB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	%B	1B	
=B	PB	VB	\B	oB	�B	�B	�B	�B	!�B	#�B	&�B	(�B	+B	-B	.B	.B	.B	.B	1'B	49B	6FB	7LB	8RB	9XB	;dB	<jB	<jB	=qB	>wB	?}B	A�B	C�B	F�B	H�B	K�B	M�B	M�B	N�B	Q�B	Q�B	R�B	VB	YB	ZB	\)B	^5B	_;B	`BB	aHB	bNB	dZB	ffB	gmB	gmB	iyB	l�B	m�B	n�B	o�B	p�B	r�B	t�B	u�B	x�B	{�B	}�B	� B	�B	�%B	�JB	�PB	�VB	�\B	�bB	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�9B	�9B	�FB	�RB	�RB	�XB	�dB	�jB	�jB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	B	B	B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�/B	�5B	�BB	�NB	�NB	�ZB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
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
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
DB
JB
PB
VB
\B
\B
\B
bB
bB
hB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
(�B
+B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
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
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B� B�4B� B�CB OB'B�B�B�B�B�B�B�B�B�NB��BߊBܬB�qBյB�B�B��B��B�AB��B��B�AB�6B��B��B� B�vB�{B�pB�dB��B��B��B��B��B��B�B��B�B{Bx�ByXB{�BpUBncBoBk�B^5BMBA;B5�B,�B-�B*�B*B�B�B�BB��B�B�B�B��B��B�B�B�ZB�xB� B�rB��B��B�B��B�VB�}B�KB�Bx�Bt�Bf�B[#BVBO(BD3B49B+B# B�B�B
��B
��B
�8B
�SB
�B
�B
�0B
�cB
��B
�;B
��B
�0B
|�B
tTB
qAB
n}B
i�B
a�B
Y�B
QhB
HKB
<�B
2GB
,qB
$@B
�B
�B
�B
PB
^B
�B
UB	�.B	�B	ߊB	�TB	�B	��B	�`B	�"B	��B	��B	��B	�dB	��B	�B	|�B	t�B	n�B	jKB	f�B	e`B	a�B	YB	X�B	YKB	T,B	P�B	F�B	AUB	>]B	<jB	9>B	4�B	3MB	1�B	1�B	-]B	&�B	"�B	pB	CB	�B	B	�B	_B	B	�B	�B	�B	B	�B	)B	1B	�B�B��B��B��B�B��B�sB�NB�CB��BՁB��B��B�SB�tB�B�uB��B�2B��B�RB��B��B��B�jB�dB�xB�#B�mB��B�B�4B��B��B�B�B��B��B�tB��B|6By�B{JBzBx�Bv�BuBr�Bq�Bo�Bh�Bg8Bc�BcBaHB`\B`\B^�B[�BY�BVmBR�BPHBJrBI7BI�BH1BESBAUB@OB;B3�B2�B4�B:^B7�B5%B3B1B0;B.IB,�B+�B+B)�B)�B(�B(�B'�B%�B$�B$�B$�B&�B(�B'�B($B'�B'8B'�B&�B&�B&�B&�B'B&�B&2B%,B#�B#:B%�B'mB(XB)*B*KB*KB*KB*eB*B+B,=B.IB1B2-B4B4�B6�B7LB7�B9>B;0B;�B<�B<�B<�B=�B=�B>(B@BBABCaBC�BE�BI7BKDBLdBMBMjBOvBQ4BR:BR:BRoBS[BS[BT{BWsBYeBZkB[�B\�B_;BaHBb�Bc�Bc�Bc�Bd�BeBgBg�BkBmBo BoOBpUBp�Bq�Bq�Bq�Bq�Br�Bs3BuZByXB|PB}�B�B��B�aB�MB�gB��B��B�+B�B��B��B��B��B��B��B�B��B�B�@B�8B�0B�)B�IB�}B�oB��B��B��B��B��B�'B��B�1B�)B�PB�BB�4B�@B՛B�qBބB��B�B�B�B�B��B��B�/B�B�B��B�+B�8B��B�B�B�<B�HB	 OB	;B	'B	[B	GB	gB	tB	�B	
�B	�B	�B	�B	B	�B	�B	B	�B	"B	$B	'B	)B	+B	-)B	./B	./B	./B	.cB	1vB	4TB	6FB	7fB	8lB	9XB	;B	<�B	<�B	=�B	>�B	?�B	A�B	C�B	F�B	IB	K�B	NB	M�B	OB	RB	R B	S&B	V9B	YKB	Z7B	\CB	^jB	_VB	`\B	a|B	b�B	d�B	f�B	g�B	g�B	i�B	l�B	m�B	n�B	o�B	p�B	r�B	t�B	vFB	y$B	|6B	~B	�4B	�aB	��B	�dB	�jB	�pB	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�
B	�
B	�
B	�XB	�0B	�6B	�=B	�CB	�}B	�[B	�GB	�GB	�MB	�MB	�MB	�TB	�TB	�TB	�nB	�zB	�RB	�lB	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	ðB	��B	�B	��B	��B	��B	��B	��B	��B	�	B	�B	��B	�B	� B	� B	��B	��B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�9B	�B	�SB	�QB	�CB	�CB	�dB	�~B	�dB	ބB	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�"B	�"B	�"B	�(B	�(B	�(B	�.B	�cB
 OB
;B
 B
'B
B
B
3B
B
?B
%B
%B
EB
+B
EB
_B
zB
fB
fB
KB
KB
KB
KB
KB
	RB
	RB
	lB

XB

XB

rB
^B
dB
jB
pB
\B
vB
vB
}B
}B
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
"�B
"�B
$B
#�B
$�B
$�B
%,B
&B
&LB
'RB
)DB
+6B
,"B
,=B
,WB
-CB
.IB
.IB
.IB
/5B
/5B
/B
0!B
0!B
0;B
0!B
0;B
0!B
0;B
0!B
0;B
0!B
0!B
0UB
1AB
1AB
1[B
1vB
2�B
4nB
5ZB
5tB
5�B
6�B
6zB
7LB
7�B
7�B
7�B
8lB
9rB
9�B
:�B
:xB
;dB
;B
;dB
;B
;B
;B
;B
<�B
<�B
<�B
=�B
=�B
>�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
OB
N�B
O�B
O�B
O�B
O�B
Q B
Q�B
Q�B
Q�B
RB
R B
RB
SB
S&B
SB
S�B
TB
TB
UB
UB
UB
U2B
VB
VB
W
B
W
B
W
B
W$B
W?B
W$B
W$B
X+B
XEB
XEB
Y1B
Y1B
Y1B
Z7B
Z7B
[WB
[=B
[=B
\CB
\CB
\CB
]IB
]IB
]IB
^OB
^OB
^OB
^jB
_VB
_pB
`vB
`�B
a|B
a|B
bhB
bhB
b�B
cnB
cnB
dZB
dtB
dtB
dtB
d�B
ezB
ezB
ezB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
hsB
hsB
h�B
h�B
i�B
i�B
iyB
iyB
iyB
jB
jB
jB
j�B
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o <XD�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201909210032152019092100321520190921003215202211182140202022111821402020221118214020201909220017192019092200171920190922001719  JA  ARFMdecpA19c                                                                20190911033704  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190910183757  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190910183759  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190910183800  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190910183800  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190910183800  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190910183800  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190910183800  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190910183801  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190910183801                      G�O�G�O�G�O�                JA  ARUP                                                                        20190910185448                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190910153101  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190920153215  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190920153215  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190921151719  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124020  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                