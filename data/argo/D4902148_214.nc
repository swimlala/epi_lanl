CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-26T18:38:01Z creation;2020-06-26T18:38:05Z conversion to V3.1;2022-11-21T05:26:48Z update;     
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
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20200626183801  20221123114513  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_214                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�$#7�Ԁ1   @�$$	{B�@;z���ݘ�d�����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@*=q@}p�@��R@��RA\)A?\)A_\)A\)A��A��A��A�z�A�z�A߮A��HA��B�
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
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�D���D���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�~�D���D���D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�O�A�VA�VA�XA�XA�S�A�Q�A�G�A��AՁA�7LA���A���A�O�A��/A��A��yA��DA��A��/A��7A��A�S�A���A���A���A�VA�/A��\A��A���A�33A���A�`BA��!A��A���A��A�dZA��`A��A� �A��A��9A�~�A�5?A���A��A�bNA���A�E�A���A�A�"�A��wA�;dA�bNA�5?A�jA�/A�p�A��yA�l�A��
A�p�A���A�t�A�A���A�ffA���A�dZA��A�
=A��^A��\A��A���A�ĜA��A�^5A���A��A��\A�Q�AA~�yA~1'A}oA{hsAz~�Ax�Au��Aq�AqApȴAp��ApffAo�Ao�AnbAlv�Ak��Aj��Ajr�AiC�Ag��AfĜAf9XAd�RAb��Aa"�A`��A`�A_l�A_"�A^�HA^~�A]��A[�AZ$�AY�FAY�AXv�AW�TAV�RAUƨAT{AR�yAQ�
AQ?}AP�AO+AM��ALn�AKƨAJv�AI�wAH�!AG��AF�AF{AD~�AC�FAB�AA��AA�PA@�/A@A�A?ƨA?�A>��A>(�A=x�A<=qA;��A;��A;7LA:�A9;dA8v�A7��A7�A6�uA6�A5�
A5&�A4��A4�HA4r�A3�mA3l�A2�RA1�A1�A0(�A/O�A.��A.9XA-�A,�yA+O�A*ȴA*9XA)A)33A(VA'�A'dZA'oA&~�A&1A%�-A%dZA$�A$r�A$$�A#ƨA#�A"^5A ��A �A�hA�Al�A"�AA�A�A�An�AG�AA�A1A�A��A��AQ�A�A\)A��A��A7LA�yA��A�AM�A5?A�
A
M�A	�A	"�AZA�At�A�!A�+AVA�AVA|�At�A�`A�AE�AC�A ��A �A �A v�A A�A $�A b@���@�K�@�n�@��@�@��@�\@�n�@�E�@�h@���@�(�@�S�@���@���@�b@�w@�R@�G�@�l�@�G�@���@�Z@�@�J@�S�@���@�Ĝ@�~�@ԓu@ҟ�@�hs@��/@Л�@�(�@϶F@Ο�@���@�Q�@ʰ!@ʗ�@�E�@��#@�p�@�&�@��@�Ĝ@ț�@�z�@�Z@Ƈ+@��
@�5?@��^@�x�@�Q�@��P@�ȴ@��@��@�j@�Z@�Q�@�  @��;@��F@�ff@�7L@��@�(�@�33@�~�@��#@���@�9X@��
@�dZ@�-@�G�@�C�@��+@�/@��j@�  @���@���@��@��@�|�@�
=@���@�E�@���@�x�@�O�@��/@�Q�@��@�n�@��@��7@��@���@��@��P@���@��\@���@�x�@�`B@�7L@���@���@�r�@�I�@��m@�l�@���@��R@��!@�M�@�$�@�@��@�x�@�=q@�@��@��@�r�@�;d@���@�v�@�E�@��T@�@��^@���@�`B@�G�@��@�%@��@�/@�z�@�b@�r�@��@�o@�~�@�@�`B@��9@��@�I�@�b@���@�l�@���@���@�V@�=q@�$�@�{@�{@�{@��@���@���@�`B@���@���@��j@���@�j@��m@�\)@���@���@��\@�n�@�V@�J@��@���@���@��7@��7@��@�hs@�7L@��@��`@���@�z�@�j@�A�@���@���@�\)@��@��\@�=q@��@���@��@��#@���@��h@��@��`@���@�j@�(�@��@~�@~v�@}`B@|�@|Z@|1@{��@{dZ@{o@z��@z~�@zJ@y��@yhs@y%@x�u@xA�@x �@w��@w;d@w
=@v�R@u�-@uO�@u?}@t��@t��@t��@t��@t�@t1@s��@s��@r��@rJ@q��@q&�@p�u@p��@pr�@p1'@pb@o�@o�;@o�P@o+@n�y@n�R@nV@n{@m��@l��@k�
@k"�@j-@i�@i��@ihs@i7L@i�@i�@h��@h�u@hb@g�;@g|�@g
=@f��@f��@f@e��@e?}@d�@dI�@c��@c��@cS�@c"�@b��@b^5@a�#@ahs@`�`@_�@_�P@_l�@_;d@_�@_�@^��@^$�@^@]@]`B@\�@\��@\��@\z�@\j@\Z@\1@[S�@Z�!@Z�!@Z=q@Y�^@X�`@Xr�@X1'@Xb@X  @W�@W|�@W;d@V�@V�+@V5?@V{@U��@U�-@U/@T�j@Tz�@TI�@T�@SC�@S@R�H@R��@R�!@RM�@RJ@Q%@P  @Ol�@O;d@N�@N�R@N��@N��@M�T@M�h@Mp�@MO�@L�@Kƨ@K33@Ko@K@J�H@J��@J-@I�#@I�#@I�#@I�^@IX@H��@H�9@HA�@G�@GK�@F�y@F�+@FV@F5?@F{@E�T@E@E�@E?}@D��@D�@D��@Dz�@DI�@D�@Cƨ@C"�@B~�@BM�@A�#@A&�@@�9@@bN@?�;@?�@?�P@?|�@?|�@?+@>��@>�y@>ȴ@>��@>5?@=�T@=@=��@=`B@=�@<�/@<��@<I�@;�m@;�@;dZ@;@:��@:��@:~�@:M�@9�#@9��@9�7@9hs@9G�@8��@8r�@8A�@81'@7��@7\)@7K�@7;d@7
=@6ȴ@6��@6v�@65?@5@5`B@5/@5/@4�/@4�j@4��@4I�@41@3S�@3o@2�H@2��@2n�@2M�@2M�@2-@2�@1x�@0Ĝ@0r�@0bN@0A�@/�;@/�P@/l�@.�y@.��@.v�@.5?@.{@-�@-�@-��@-��@-p�@-?}@-�@,�@,�D@,�@+ƨ@+��@+S�@+"�@*��@*~�@*=q@*J@)�^@)G�@(��@(�u@(Q�@'��@'l�@'�@&��@&ff@&@%@%p�@%�@$�/@$�@$I�@$1@#��@#�
@#ƨ@#�F@#��@#�@#t�@#dZ@#C�@#o@"�@"��@"��@"M�@!�@!�7@!X@!7L@!%@ ��@ Ĝ@ 1'@�@�w@��@�P@K�@�y@�+@$�@$�@{@�@��@`B@V@��@�@��@j@�m@��@��@t�@dZ@S�@C�@o@��@��@~�@^5@�@�@��@�7@%@��@A�@b@�;@l�@K�@ȴ@��@�+@��@�+@@@`B@��@��@z�@j@I�@(�@1@�
@t�@��@�!@�\@~�@n�@n�@n�@M�@=q@J@�#@��@hs@G�@�@��@Ĝ@��@�u@bN@Q�@A�@A�@1'@1'@ �@��@l�@\)@\)@K�@K�@+@��@��@5?@@�@�T@�-@�h@`B@V@�/@�j@9X@�F@��@��@�@t�@S�@C�@C�@
�@
��@
�!@
��@
�\@
n�@
M�@
-@	�@	��@	��@	x�@	hs@	hs@	X@	&�@	%@�`@Ĝ@�u@�u@bN@1'@b@  @�@��@�w@�@��@�P@\)@�y@v�@ff@E�@{@�T@��@p�@O�@?}@�@V@V@�@�D@Z@I�@I�@9X@��@��@�
@��@dZ@"�@�@��@��@�!@�!@��@�\@~�@^5@M�@=q@-@��@�#@��@�7@hs@X@G�@&�@%@ ��@ ��@ �9@ �@ bN@ 1'?��w?���?���?�|�?�\)?��?���?�v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�O�A�VA�VA�XA�XA�S�A�Q�A�G�A��AՁA�7LA���A���A�O�A��/A��A��yA��DA��A��/A��7A��A�S�A���A���A���A�VA�/A��\A��A���A�33A���A�`BA��!A��A���A��A�dZA��`A��A� �A��A��9A�~�A�5?A���A��A�bNA���A�E�A���A�A�"�A��wA�;dA�bNA�5?A�jA�/A�p�A��yA�l�A��
A�p�A���A�t�A�A���A�ffA���A�dZA��A�
=A��^A��\A��A���A�ĜA��A�^5A���A��A��\A�Q�AA~�yA~1'A}oA{hsAz~�Ax�Au��Aq�AqApȴAp��ApffAo�Ao�AnbAlv�Ak��Aj��Ajr�AiC�Ag��AfĜAf9XAd�RAb��Aa"�A`��A`�A_l�A_"�A^�HA^~�A]��A[�AZ$�AY�FAY�AXv�AW�TAV�RAUƨAT{AR�yAQ�
AQ?}AP�AO+AM��ALn�AKƨAJv�AI�wAH�!AG��AF�AF{AD~�AC�FAB�AA��AA�PA@�/A@A�A?ƨA?�A>��A>(�A=x�A<=qA;��A;��A;7LA:�A9;dA8v�A7��A7�A6�uA6�A5�
A5&�A4��A4�HA4r�A3�mA3l�A2�RA1�A1�A0(�A/O�A.��A.9XA-�A,�yA+O�A*ȴA*9XA)A)33A(VA'�A'dZA'oA&~�A&1A%�-A%dZA$�A$r�A$$�A#ƨA#�A"^5A ��A �A�hA�Al�A"�AA�A�A�An�AG�AA�A1A�A��A��AQ�A�A\)A��A��A7LA�yA��A�AM�A5?A�
A
M�A	�A	"�AZA�At�A�!A�+AVA�AVA|�At�A�`A�AE�AC�A ��A �A �A v�A A�A $�A b@���@�K�@�n�@��@�@��@�\@�n�@�E�@�h@���@�(�@�S�@���@���@�b@�w@�R@�G�@�l�@�G�@���@�Z@�@�J@�S�@���@�Ĝ@�~�@ԓu@ҟ�@�hs@��/@Л�@�(�@϶F@Ο�@���@�Q�@ʰ!@ʗ�@�E�@��#@�p�@�&�@��@�Ĝ@ț�@�z�@�Z@Ƈ+@��
@�5?@��^@�x�@�Q�@��P@�ȴ@��@��@�j@�Z@�Q�@�  @��;@��F@�ff@�7L@��@�(�@�33@�~�@��#@���@�9X@��
@�dZ@�-@�G�@�C�@��+@�/@��j@�  @���@���@��@��@�|�@�
=@���@�E�@���@�x�@�O�@��/@�Q�@��@�n�@��@��7@��@���@��@��P@���@��\@���@�x�@�`B@�7L@���@���@�r�@�I�@��m@�l�@���@��R@��!@�M�@�$�@�@��@�x�@�=q@�@��@��@�r�@�;d@���@�v�@�E�@��T@�@��^@���@�`B@�G�@��@�%@��@�/@�z�@�b@�r�@��@�o@�~�@�@�`B@��9@��@�I�@�b@���@�l�@���@���@�V@�=q@�$�@�{@�{@�{@��@���@���@�`B@���@���@��j@���@�j@��m@�\)@���@���@��\@�n�@�V@�J@��@���@���@��7@��7@��@�hs@�7L@��@��`@���@�z�@�j@�A�@���@���@�\)@��@��\@�=q@��@���@��@��#@���@��h@��@��`@���@�j@�(�@��@~�@~v�@}`B@|�@|Z@|1@{��@{dZ@{o@z��@z~�@zJ@y��@yhs@y%@x�u@xA�@x �@w��@w;d@w
=@v�R@u�-@uO�@u?}@t��@t��@t��@t��@t�@t1@s��@s��@r��@rJ@q��@q&�@p�u@p��@pr�@p1'@pb@o�@o�;@o�P@o+@n�y@n�R@nV@n{@m��@l��@k�
@k"�@j-@i�@i��@ihs@i7L@i�@i�@h��@h�u@hb@g�;@g|�@g
=@f��@f��@f@e��@e?}@d�@dI�@c��@c��@cS�@c"�@b��@b^5@a�#@ahs@`�`@_�@_�P@_l�@_;d@_�@_�@^��@^$�@^@]@]`B@\�@\��@\��@\z�@\j@\Z@\1@[S�@Z�!@Z�!@Z=q@Y�^@X�`@Xr�@X1'@Xb@X  @W�@W|�@W;d@V�@V�+@V5?@V{@U��@U�-@U/@T�j@Tz�@TI�@T�@SC�@S@R�H@R��@R�!@RM�@RJ@Q%@P  @Ol�@O;d@N�@N�R@N��@N��@M�T@M�h@Mp�@MO�@L�@Kƨ@K33@Ko@K@J�H@J��@J-@I�#@I�#@I�#@I�^@IX@H��@H�9@HA�@G�@GK�@F�y@F�+@FV@F5?@F{@E�T@E@E�@E?}@D��@D�@D��@Dz�@DI�@D�@Cƨ@C"�@B~�@BM�@A�#@A&�@@�9@@bN@?�;@?�@?�P@?|�@?|�@?+@>��@>�y@>ȴ@>��@>5?@=�T@=@=��@=`B@=�@<�/@<��@<I�@;�m@;�@;dZ@;@:��@:��@:~�@:M�@9�#@9��@9�7@9hs@9G�@8��@8r�@8A�@81'@7��@7\)@7K�@7;d@7
=@6ȴ@6��@6v�@65?@5@5`B@5/@5/@4�/@4�j@4��@4I�@41@3S�@3o@2�H@2��@2n�@2M�@2M�@2-@2�@1x�@0Ĝ@0r�@0bN@0A�@/�;@/�P@/l�@.�y@.��@.v�@.5?@.{@-�@-�@-��@-��@-p�@-?}@-�@,�@,�D@,�@+ƨ@+��@+S�@+"�@*��@*~�@*=q@*J@)�^@)G�@(��@(�u@(Q�@'��@'l�@'�@&��@&ff@&@%@%p�@%�@$�/@$�@$I�@$1@#��@#�
@#ƨ@#�F@#��@#�@#t�@#dZ@#C�@#o@"�@"��@"��@"M�@!�@!�7@!X@!7L@!%@ ��@ Ĝ@ 1'@�@�w@��@�P@K�@�y@�+@$�@$�@{@�@��@`B@V@��@�@��@j@�m@��@��@t�@dZ@S�@C�@o@��@��@~�@^5@�@�@��@�7@%@��@A�@b@�;@l�@K�@ȴ@��@�+@��@�+@@@`B@��@��@z�@j@I�@(�@1@�
@t�@��@�!@�\@~�@n�@n�@n�@M�@=q@J@�#@��@hs@G�@�@��@Ĝ@��@�u@bN@Q�@A�@A�@1'@1'@ �@��@l�@\)@\)@K�@K�@+@��@��@5?@@�@�T@�-@�h@`B@V@�/@�j@9X@�F@��@��@�@t�@S�@C�@C�@
�@
��@
�!@
��@
�\@
n�@
M�@
-@	�@	��@	��@	x�@	hs@	hs@	X@	&�@	%@�`@Ĝ@�u@�u@bN@1'@b@  @�@��@�w@�@��@�P@\)@�y@v�@ff@E�@{@�T@��@p�@O�@?}@�@V@V@�@�D@Z@I�@I�@9X@��@��@�
@��@dZ@"�@�@��@��@�!@�!@��@�\@~�@^5@M�@=q@-@��@�#@��@�7@hs@X@G�@&�@%@ ��@ ��@ �9@ �@ bN@ 1'?��w?���?���?�|�?�\)?��?���?�v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BT�BT�BT�BT�BS�BS�BS�BS�BR�BP�BK�BB�B1'B�B��B�TB��BǮB�RB�B��B��B�oB�JB�=B�Bx�Bv�Bw�Bt�BiyB[#BZBZBXBS�BM�BG�BA�B:^B33B+B�BhBPB	7BB��B�B�B�`B�BĜB�RB�B��B��B�oB�Bt�Bq�BhsBaHBXBO�BI�BA�B;dB49B-B�B�BJBB
��B
�B
�B
�ZB
��B
B
�B
��B
�JB
�B
z�B
u�B
o�B
hsB
bNB
YB
M�B
G�B
<jB
&�B
bB
	7B
+B
%B
B	��B	��B	��B	��B	��B	�B	�B	�mB	�BB	�B	�B	��B	ĜB	�jB	�^B	�LB	�9B	�-B	�!B	�B	��B	��B	��B	�oB	�\B	�JB	�7B	�B	|�B	u�B	o�B	jB	gmB	e`B	^5B	XB	Q�B	P�B	K�B	G�B	C�B	?}B	;dB	7LB	1'B	.B	)�B	%�B	#�B	 �B	�B	�B	�B	�B	{B	hB	PB	DB		7B	+B	B��B��B��B��B�B�B�B�B�B�B�sB�fB�TB�BB�)B�B��B��B��B��BɺBŢB�}B�jB�^B�RB�?B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B�uB�bB�PB�=B�%B�B�B~�B{�By�Bu�Bq�Bn�Bk�BgmBe`BcTBbNBaHB_;B\)BW
BT�BS�BS�BR�BQ�BP�BN�BL�BJ�BI�BG�BF�BD�BC�BC�BB�B@�B>wB<jB<jB;dB:^B9XB8RB7LB7LB6FB6FB6FB5?B5?B49B33B2-B.B,B+B)�B)�B(�B(�B'�B&�B&�B%�B%�B%�B$�B$�B#�B"�B"�B"�B!�B�B!�B!�B �B �B!�B!�B"�B$�B$�B$�B#�B#�B#�B#�B$�B%�B%�B%�B%�B&�B&�B&�B&�B&�B&�B%�B'�B,B/B1'B1'B6FB7LB9XB=qB=qB=qB=qB=qB=qB=qB=qB@�BB�BE�BF�BI�BK�BK�BM�BO�BQ�BT�BW
BXB^5B`BBbNBcTBdZBe`BffBjBl�Bm�Bn�Bn�Bo�Bo�Bp�Bp�Bp�Bp�Bs�Bs�Bt�Bv�Bw�Bw�By�By�B�B�VB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�XB�XB�LB�FB�LB�RB�XB�dB�jB�jB�wBBÖBÖBĜBĜBŢB��B��B�B�B�
B�
B�B�B�)B�/B�;B�HB�ZB�mB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	
=B	PB	\B	bB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	$�B	'�B	(�B	.B	2-B	5?B	6FB	8RB	9XB	:^B	<jB	=qB	A�B	C�B	E�B	G�B	I�B	L�B	N�B	O�B	T�B	VB	YB	ZB	\)B	\)B	^5B	_;B	`BB	aHB	bNB	cTB	e`B	gmB	hsB	hsB	jB	l�B	m�B	n�B	p�B	q�B	r�B	s�B	t�B	t�B	t�B	u�B	y�B	y�B	z�B	{�B	}�B	�B	�B	�1B	�7B	�=B	�DB	�DB	�JB	�JB	�PB	�\B	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�LB	�LB	�RB	�XB	�dB	�jB	�wB	��B	��B	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B

=B
DB
JB
PB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
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
%�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
2-B
33B
33B
49B
49B
49B
49B
5?B
49B
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
:^B
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
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
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
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
e`B
e`B
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
gmB
gmB
gmB
gmB
gmB
hsB
iyB
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
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
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
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BT�BT�BT�BT�BS�BTBTBTFBS�BR�BOvBH�B9rB'�BdB�6BںB��B��B��B��B��B��B��B�B�KBz�ByXB{JBw�BmCB^B[=B[WBY�BVmBO�BIRBC-B;�B5?B-CB!�BB�B
#B�B��B��B��B�RB��B��B�*B�IB�sB��B��B��Bu�Bs3Bi�Bb�BYeBQ BKBB�B<�B5�B/�B�B
B�B[B
��B
�B
��B
�8B
�@B
�SB
��B
�kB
��B
��B
{�B
v�B
p�B
i�B
c�B
[	B
OvB
J=B
@4B
*eB
�B
	�B
zB
�B
�B
 4B	�dB	��B	��B	��B	��B	�!B	�B	��B	�=B	�B	�NB	�%B	�<B	�B	�B	��B	��B	��B	��B	�yB	��B	�9B	�@B	�HB	�PB	��B	��B	~�B	w2B	p�B	kkB	hsB	gmB	`B	YB	SB	RoB	L�B	IB	D�B	AB	<�B	9$B	2GB	/5B	+B	&�B	$�B	!�B	pB	IB	�B	B	�B	�B	�B	�B	
	B	�B	aB	  B��B��B��B�MB�'B�iB��B��B�QB�DB�8B�tB�bB�dB�QB�B��BΥB̘B�DB�_B�OB�<B�B�>B�FB��B��B��B��B��B�yB�mB��B��B�NB�vB��B��B�7B��B�NB��B��B��B�9B�B�B}B{�Bw�Bs3Bo�Bm�BhXBf�Bc�Bc BbNB`�B^�BX�BU�BTFBTFBSuBRTBQ�BP�BM�BKxBJ�BHfBGzBE�BC�BDBC�BB�B?}B<�B=<B<B:�B:xB8�B7�B7�B6�B6�B6�B5�B5�B4�B4nB4�B1vB-�B+kB*KB*KB)�B)�B(�B'�B'�B&�B&�B&�B%�B%�B%,B$&B#nB#�B#nB"�B#nB"�B"hB"hB#TB# B#�B%`B%,B%FB$ZB$�B$�B%B%�B&2B&2B&fB&LB'8B'8B'8B'8B'RB'�B'mB)�B-)B/�B1�B2B6�B8B:�B=�B=�B=�B=�B=�B=�B=�B>�BAoBC-BFBG_BJrBLdBL�BNpBPbBR�BU�BW�BYB^�Ba-Bb�BdBe,BfLBg8Bk6Bl�Bm�Bo Bn�BpBp!Bp�Bq'BqABq�Bt9Bt9Bu%Bw2Bx8Bx�Bz�B{B��B��B��B��B��B��B��B��B��B�B�	B�B��B�>B�KB�*B�>B�>B��B��B��B��B��B�*B�B��B��B��B��B�B��B��B��BªBðB��BĜB��B�%B�)B��B֡BخB׍BרB�yBؓB�]B�dB�pB�B�B��B��B��B��B��B��B��B��B��B��B��B�B�*B�(B�B�HB�cB	�B	�B	
�B	�B	�B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	# B	%,B	(>B	)DB	.cB	2|B	5tB	6`B	8lB	9rB	:�B	<�B	=�B	A�B	C�B	E�B	G�B	J	B	MB	OB	PHB	UMB	V9B	YKB	ZkB	\CB	\xB	^OB	_pB	`vB	a|B	b�B	c�B	e�B	g�B	h�B	h�B	j�B	l�B	m�B	n�B	p�B	q�B	r�B	s�B	t�B	t�B	t�B	vB	y�B	y�B	{0B	|6B	~(B	�aB	�SB	�KB	�lB	�XB	�^B	�^B	�dB	�~B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�2B	�$B	�B	�0B	�6B	�)B	�CB	�IB	�UB	�[B	�aB	��B	��B	�fB	�fB	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	� B	� B	�B	�:B	�,B	�9B	�9B	�EB	�eB	�xB	�IB	�OB	�VB	�pB	�vB	�vB	�|B	�B	�nB	�tB	�B	�zB	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�>B	��B	�B	�B	�"B	�VB
;B
AB
'B
'B
-B
aB
3B
B
B
SB
mB
tB
fB
fB
	lB

�B
�B
~B
jB
pB
pB
pB
vB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#B
#B
$B
$B
%B
$�B
$�B
%�B
&B
&B
&2B
'B
(
B
(
B
(
B
($B
)B
*B
*B
*0B
*0B
+B
+B
+B
,"B
,"B
,"B
,"B
-]B
./B
./B
/B
/5B
/5B
0;B
0;B
1AB
1[B
2GB
3MB
3MB
4TB
4TB
4TB
4TB
5ZB
4�B
6zB
7fB
7�B
7fB
7�B
8lB
9rB
9�B
:xB
:�B
:xB
;B
;B
;dB
;B
;B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
NB
NB
N�B
N�B
N�B
OB
OB
O�B
Q B
P�B
Q B
Q B
RB
QB
R B
S&B
SB
TB
TB
T,B
UB
VB
VB
VB
VB
VB
VB
W?B
W$B
W$B
W$B
W?B
X+B
X+B
X+B
XEB
YKB
Y1B
Z7B
Z7B
ZQB
Z7B
ZQB
Z7B
[=B
[#B
[=B
[qB
\CB
\]B
\]B
]IB
]IB
]/B
]dB
]IB
^OB
^OB
^jB
_pB
`\B
`\B
`BB
abB
aHB
aHB
abB
abB
a|B
a|B
abB
bhB
bhB
bhB
bhB
cnB
cnB
cTB
cnB
cTB
cTB
cTB
cTB
dZB
cnB
cnB
dtB
e`B
e`B
e`B
e`B
e�B
ezB
e�B
f�B
f�B
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
i�B
jB
jB
jB
jB
j�B
jB
jB
j�B
k�B
k�B
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
m�B
m�B
m�B
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
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
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
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y	B
x�B
y	B
x�B
y�B
y�B
zB
zB
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<V�b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202007070035492020070700354920200707003549202211182143362022111821433620221118214336202007080019232020070800192320200708001923  JA  ARFMdecpA19c                                                                20200627033800  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200626183801  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200626183804  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200626183804  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200626183805  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200626183805  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200626183805  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200626183805  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200626183805  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200626183805  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200626183805  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200626183805                      G�O�G�O�G�O�                JA  ARUP                                                                        20200626185324                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200627153120  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200627153050  CV  JULD            G�O�G�O�F�!                JM  ARCAJMQC2.0                                                                 20200706153549  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200706153549  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200707151923  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124336  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114513                      G�O�G�O�G�O�                