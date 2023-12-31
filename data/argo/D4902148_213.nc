CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-16T15:39:35Z creation;2020-06-16T15:39:39Z conversion to V3.1;2022-11-21T05:26:51Z update;     
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
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20200616153935  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_213                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�!�W�� 1   @�!�W; @;hXy=��d���2�X1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C~\C��C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD	}qD	�qD
}qD
�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�
D}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD}qD�qD }qD �qD!}qD!�qD"}qD"�qD#}qD#�qD$}qD$�qD%}qD%�qD&}qD&�qD'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD+�qD,}qD,�qD-}qD-�qD.}qD.�qD/}qD/�qD0}qD0�qD1}qD1�qD2}qD2�qD3}qD3�qD4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD8�qD9}qD9�qD:}qD:�qD;}qD<�D<}qD<�qD=}qD=�qD>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDB}qDB�qDC}qDC�qDD}qDD�qDE}qDE�qDF}qDF�qDG}qDG�qDH}qDH�qDI}qDI�qDJ}qDJ�qDK}qDK�qDL}qDL�qDM}qDM�qDN}qDN�qDO}qDO�qDP}qDP�qDQ}qDQ�qDR}qDR�qDS}qDS�qDT}qDT�qDU}qDU�qDV}qDV�qDW}qDW�qDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD[�qD\}qD\�qD]}qD]�qD^}qD^�qD_}qD_�qD`}qD`�qDa}qDa�qDb}qDb�qDc}qDc�qDd}qDd�qDe}qDe�qDf}qDf�qDg}qDg�qDh}qDh�qDi}qDi�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm�qDn}qDn�qDo}qDo�qDp}qDp�qDq}qDq�qDr}qDr�qDs}qDs�qDt}qDt�qDu}qDu�qDv}qDv�qDw}qDw�qDx}qDx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD|�qD}}qD}�qD~}qD~�qD}qD�qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�>�D�~�Dž�D���D�>�D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�~�DȾ�D���D�>�D�~�Dɾ�D���D�>�D�~�Dʾ�D���D�>�D�~�D˾�D���D�>�D�~�D̾�D���D�>�D�~�D;�D���D�>�D�~�Dξ�D���D�>�D�~�DϾ�D���D�>�D�~�Dо�D���D�>�D�~�DѾ�D���D�>�D�~�DҾ�D���D�>�D�~�DӾ�D���D�>�D�~�DԾ�D���D�>�D�~�Dվ�D���D�>�D�~�D־�D���D�>�D�~�D׾�D���D�>�D�~�Dؾ�D���D�>�D�~�Dپ�D���D�>�D�~�Dھ�D���D�>�D�~�D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D�~�D߾�D���D�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�~�D㾸D���D�>�D�~�D侸D���D�>�D�~�D徸D���D�>�D�~�D澸D���D�>�D�~�D羸D���D�>�D�~�D辸D���D�>�D�~�D龸D���D�>�D�~�D꾸D���D�>�D�{�D뾸D��D�>�D�~�D쾸D���D�>�D�~�D���D���D�>�D�~�DD���D�>�D�~�DﾸD���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D�D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A�(�A�(�A��A���A���A��Aƣ�A�A���A��A��\A��^A���A��9A��A��A�bNA�ȴA�=qA�dZA�9XA�=qA�ffA���A�E�A�n�A�VA�ĜA�=qA�VA�I�A�  A�A���A�z�A�"�A�bNA���A��A�dZA��FA�
=A�\)A��DA���A�dZA���A�dZA��A�bA�Q�A�
=A�A���A��A�bNA�/A�bA��A�Q�A��^A�ȴA��DA�S�A�ȴA�^5A���A�v�A�dZA��yA��uA�5?A�$�A�VA��`A��hA�;dA���A�
=A�\)A�1A���A�~�A�"�A���A���A��A��uA�-A~�DA}�Az��Ay��AxbAw
=Av9XAu�At��AsAqhsAp�An9XAk��Ak�AiAhJAfn�Aep�Ad��Ac�FAaA`��A`  A_G�A^�RA]�A\�/A\��A\JAZ1'AY��AY��AYhsAX��AWAW�AU��AS�TARbNAP�AO��AN��AM�mALr�AKO�AJ(�AIAHbNAGAE��AC��AB1'AA�A@�\A?XA>^5A<�A:�A9�#A9l�A7��A6Q�A4bNA3��A3l�A2��A25?A1�#A1��A1p�A0��A0M�A/�FA.��A.=qA.  A-x�A,r�A+�PA+&�A*��A*��A*n�A*ZA)��A)��A(�A(I�A(�A(A'�A'��A'�A&��A&�uA&ZA%��A$(�A"�`A"��A"ZA!��A!|�A!O�A!33A �`A ��A -A��Al�AbNA1A��At�AVA�\A��AȴA�-A�uA �AAdZAVA�uA/A&�A�9A�uAA�!A$�At�A�A�AM�A�mA�7A
��A	��A	\)A	33A��A1A�;A|�A�AQ�AAƨAx�A;dA+AoA ȴA z�A I�@��@��@�ff@�r�@�7L@�A�@��@�V@��T@��;@��@�"�@�Ĝ@�C�@��y@��@���@���@�!@柾@�~�@�v�@�V@��/@⟾@�$�@ާ�@܋D@��y@��`@�dZ@�v�@�1'@���@��@�\)@�^5@�`B@�%@̣�@�  @�\)@��@�1'@��@�o@��#@���@�ƨ@�
=@���@�ff@�E�@��^@���@��F@��@���@���@�bN@� �@���@��@��@���@�hs@���@�b@�\)@�"�@��T@�&�@���@���@�j@��
@���@�V@�X@��u@��w@�"�@��h@�Ĝ@�A�@���@�S�@���@���@��\@�E�@���@�hs@���@��;@�l�@���@�G�@���@��@���@�@��+@���@��/@��@���@�K�@���@���@�5?@���@��@��9@�Z@� �@��w@�"�@���@�M�@�@���@�V@��9@��u@�j@�I�@�A�@�9X@�(�@� �@� �@���@�\)@�@���@�5?@�J@��@��^@��@���@��j@���@�r�@�A�@�A�@�9X@�1'@�1@�C�@��!@��+@�n�@��@��@��^@��-@���@���@���@��h@��7@�hs@��@�r�@�I�@�9X@�b@��@�|�@�S�@�
=@���@���@�M�@�{@��T@���@�/@��`@�Ĝ@�Z@��m@���@�t�@��y@�n�@�=q@��@���@��h@��@�X@��@�%@���@��@��@�j@�bN@�bN@�bN@�I�@�  @��w@���@�K�@��H@�~�@�E�@�$�@�{@��@�@��@�O�@�O�@�O�@�&�@���@�r�@�Z@�@~��@}�@|��@{ƨ@{"�@z�@y�#@y��@y&�@x��@x�@x�@xr�@x �@w;d@vȴ@v5?@u?}@tz�@sC�@r�\@r-@q��@qG�@p�9@p��@p�u@pr�@o�;@o��@o|�@o\)@o�@o
=@nȴ@n�+@m�@mO�@m/@mV@l�/@lj@l9X@l�@kC�@j�\@jn�@i��@iG�@h�u@h �@g\)@g;d@g
=@f��@f5?@f{@e�T@ep�@e?}@eV@d�/@d��@d�@d�D@dZ@c��@b�!@b^5@b-@a�#@a7L@`�9@`  @_|�@_+@_
=@^��@^ff@^@]�@]/@]V@\�j@[��@[C�@["�@Z�@ZJ@Y�@XĜ@X�u@X�@X�u@X�u@X�@X �@W�@W\)@WK�@W
=@V�@V�+@VV@V$�@U�T@U@U�h@Up�@U`B@UO�@UO�@U?}@U/@U�@U�@T�@TZ@St�@S33@S33@R��@R�!@R~�@R�@Q��@QG�@PĜ@PbN@O�w@O�@N��@N�@NV@M�@M@M��@Mp�@L��@L�j@L�@L��@LZ@L�@K�
@K��@Kt�@KS�@K@J�!@J�@IG�@I�@H��@H��@HĜ@H��@H��@H�u@HbN@G�w@GK�@G
=@F�@F��@Fv�@FV@F$�@E�-@E?}@E/@D��@D�@Dj@D9X@D�@C�F@B�!@A�^@A&�@@�`@@�9@@Q�@@ �@@ �@@ �@@b@@  @@  @?�@?�@?�;@?��@?�@?��@?\)@>��@>�y@>ȴ@>��@>��@>�+@>�+@>�+@>�+@>E�@=��@=O�@<�/@;�m@;33@;@:�H@:�!@:�\@:n�@:n�@:M�@:J@9�#@9�7@9x�@9&�@8r�@81'@7�w@7�P@7l�@7l�@7K�@7+@7
=@6ȴ@6E�@5�T@5/@4�D@3�F@3dZ@333@3@2�@2��@2�!@2-@1�#@1x�@1X@1X@1G�@1&�@1%@0��@0�`@0�9@0�9@0Q�@0b@/�@/��@/��@/l�@/K�@/;d@/
=@.�@.E�@-�h@-`B@-V@,�@,j@+��@*�@*�!@*n�@*^5@*=q@*=q@*=q@*=q@*-@*�@)��@)�@)�^@)x�@)&�@(��@(r�@'�@'�;@'��@'�w@'+@&��@&v�@&E�@%�T@%`B@%?}@%�@$��@$�/@$z�@$j@$Z@$Z@#ƨ@#C�@#C�@#33@#"�@#o@"��@"�\@"=q@!�#@!G�@!%@!%@ ��@ �`@ �9@ �@ bN@ bN@ bN@ bN@ Q�@ A�@ 1'@ b@�;@�P@|�@;d@
=@ȴ@��@V@@�T@�T@��@��@�@/@V@V@�@�/@��@��@�j@�D@z�@I�@�m@�F@��@��@33@��@n�@n�@n�@^5@=q@��@��@��@x�@X@X@G�@&�@�9@�@bN@ �@��@K�@ȴ@��@5?@�-@p�@?}@V@�@�/@�@��@��@z�@I�@1@��@�@t�@C�@@��@~�@-@�@�@�^@��@�7@hs@&�@��@Ĝ@�u@��@�u@r�@1'@�@�@�P@�P@l�@��@��@�y@�@�@��@��@v�@ff@ff@V@5?@$�@{@@@�@�T@@�-@�-@�-@�-@�-@�h@p�@?}@/@/@�@�@�
@�@�@dZ@S�@C�@o@
�H@
��@
~�@
-@	��@	�#@	��@	X@	&�@	%@��@��@��@�9@�u@�@A�@b@b@b@  @�w@�P@|�@K�@+@
=@�y@ȴ@�R@�R@�+@V@�T@@��@�h@`B@?}@V@�@�j@�D@Z@(�@��@�m@ƨ@��@�@C�@"�@��@�\@�@�@�^@�7@X@7L@&�@ �9@  �?��w?�|�?�;d?��?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A� �A�(�A�(�A��A���A���A��Aƣ�A�A���A��A��\A��^A���A��9A��A��A�bNA�ȴA�=qA�dZA�9XA�=qA�ffA���A�E�A�n�A�VA�ĜA�=qA�VA�I�A�  A�A���A�z�A�"�A�bNA���A��A�dZA��FA�
=A�\)A��DA���A�dZA���A�dZA��A�bA�Q�A�
=A�A���A��A�bNA�/A�bA��A�Q�A��^A�ȴA��DA�S�A�ȴA�^5A���A�v�A�dZA��yA��uA�5?A�$�A�VA��`A��hA�;dA���A�
=A�\)A�1A���A�~�A�"�A���A���A��A��uA�-A~�DA}�Az��Ay��AxbAw
=Av9XAu�At��AsAqhsAp�An9XAk��Ak�AiAhJAfn�Aep�Ad��Ac�FAaA`��A`  A_G�A^�RA]�A\�/A\��A\JAZ1'AY��AY��AYhsAX��AWAW�AU��AS�TARbNAP�AO��AN��AM�mALr�AKO�AJ(�AIAHbNAGAE��AC��AB1'AA�A@�\A?XA>^5A<�A:�A9�#A9l�A7��A6Q�A4bNA3��A3l�A2��A25?A1�#A1��A1p�A0��A0M�A/�FA.��A.=qA.  A-x�A,r�A+�PA+&�A*��A*��A*n�A*ZA)��A)��A(�A(I�A(�A(A'�A'��A'�A&��A&�uA&ZA%��A$(�A"�`A"��A"ZA!��A!|�A!O�A!33A �`A ��A -A��Al�AbNA1A��At�AVA�\A��AȴA�-A�uA �AAdZAVA�uA/A&�A�9A�uAA�!A$�At�A�A�AM�A�mA�7A
��A	��A	\)A	33A��A1A�;A|�A�AQ�AAƨAx�A;dA+AoA ȴA z�A I�@��@��@�ff@�r�@�7L@�A�@��@�V@��T@��;@��@�"�@�Ĝ@�C�@��y@��@���@���@�!@柾@�~�@�v�@�V@��/@⟾@�$�@ާ�@܋D@��y@��`@�dZ@�v�@�1'@���@��@�\)@�^5@�`B@�%@̣�@�  @�\)@��@�1'@��@�o@��#@���@�ƨ@�
=@���@�ff@�E�@��^@���@��F@��@���@���@�bN@� �@���@��@��@���@�hs@���@�b@�\)@�"�@��T@�&�@���@���@�j@��
@���@�V@�X@��u@��w@�"�@��h@�Ĝ@�A�@���@�S�@���@���@��\@�E�@���@�hs@���@��;@�l�@���@�G�@���@��@���@�@��+@���@��/@��@���@�K�@���@���@�5?@���@��@��9@�Z@� �@��w@�"�@���@�M�@�@���@�V@��9@��u@�j@�I�@�A�@�9X@�(�@� �@� �@���@�\)@�@���@�5?@�J@��@��^@��@���@��j@���@�r�@�A�@�A�@�9X@�1'@�1@�C�@��!@��+@�n�@��@��@��^@��-@���@���@���@��h@��7@�hs@��@�r�@�I�@�9X@�b@��@�|�@�S�@�
=@���@���@�M�@�{@��T@���@�/@��`@�Ĝ@�Z@��m@���@�t�@��y@�n�@�=q@��@���@��h@��@�X@��@�%@���@��@��@�j@�bN@�bN@�bN@�I�@�  @��w@���@�K�@��H@�~�@�E�@�$�@�{@��@�@��@�O�@�O�@�O�@�&�@���@�r�@�Z@�@~��@}�@|��@{ƨ@{"�@z�@y�#@y��@y&�@x��@x�@x�@xr�@x �@w;d@vȴ@v5?@u?}@tz�@sC�@r�\@r-@q��@qG�@p�9@p��@p�u@pr�@o�;@o��@o|�@o\)@o�@o
=@nȴ@n�+@m�@mO�@m/@mV@l�/@lj@l9X@l�@kC�@j�\@jn�@i��@iG�@h�u@h �@g\)@g;d@g
=@f��@f5?@f{@e�T@ep�@e?}@eV@d�/@d��@d�@d�D@dZ@c��@b�!@b^5@b-@a�#@a7L@`�9@`  @_|�@_+@_
=@^��@^ff@^@]�@]/@]V@\�j@[��@[C�@["�@Z�@ZJ@Y�@XĜ@X�u@X�@X�u@X�u@X�@X �@W�@W\)@WK�@W
=@V�@V�+@VV@V$�@U�T@U@U�h@Up�@U`B@UO�@UO�@U?}@U/@U�@U�@T�@TZ@St�@S33@S33@R��@R�!@R~�@R�@Q��@QG�@PĜ@PbN@O�w@O�@N��@N�@NV@M�@M@M��@Mp�@L��@L�j@L�@L��@LZ@L�@K�
@K��@Kt�@KS�@K@J�!@J�@IG�@I�@H��@H��@HĜ@H��@H��@H�u@HbN@G�w@GK�@G
=@F�@F��@Fv�@FV@F$�@E�-@E?}@E/@D��@D�@Dj@D9X@D�@C�F@B�!@A�^@A&�@@�`@@�9@@Q�@@ �@@ �@@ �@@b@@  @@  @?�@?�@?�;@?��@?�@?��@?\)@>��@>�y@>ȴ@>��@>��@>�+@>�+@>�+@>�+@>E�@=��@=O�@<�/@;�m@;33@;@:�H@:�!@:�\@:n�@:n�@:M�@:J@9�#@9�7@9x�@9&�@8r�@81'@7�w@7�P@7l�@7l�@7K�@7+@7
=@6ȴ@6E�@5�T@5/@4�D@3�F@3dZ@333@3@2�@2��@2�!@2-@1�#@1x�@1X@1X@1G�@1&�@1%@0��@0�`@0�9@0�9@0Q�@0b@/�@/��@/��@/l�@/K�@/;d@/
=@.�@.E�@-�h@-`B@-V@,�@,j@+��@*�@*�!@*n�@*^5@*=q@*=q@*=q@*=q@*-@*�@)��@)�@)�^@)x�@)&�@(��@(r�@'�@'�;@'��@'�w@'+@&��@&v�@&E�@%�T@%`B@%?}@%�@$��@$�/@$z�@$j@$Z@$Z@#ƨ@#C�@#C�@#33@#"�@#o@"��@"�\@"=q@!�#@!G�@!%@!%@ ��@ �`@ �9@ �@ bN@ bN@ bN@ bN@ Q�@ A�@ 1'@ b@�;@�P@|�@;d@
=@ȴ@��@V@@�T@�T@��@��@�@/@V@V@�@�/@��@��@�j@�D@z�@I�@�m@�F@��@��@33@��@n�@n�@n�@^5@=q@��@��@��@x�@X@X@G�@&�@�9@�@bN@ �@��@K�@ȴ@��@5?@�-@p�@?}@V@�@�/@�@��@��@z�@I�@1@��@�@t�@C�@@��@~�@-@�@�@�^@��@�7@hs@&�@��@Ĝ@�u@��@�u@r�@1'@�@�@�P@�P@l�@��@��@�y@�@�@��@��@v�@ff@ff@V@5?@$�@{@@@�@�T@@�-@�-@�-@�-@�-@�h@p�@?}@/@/@�@�@�
@�@�@dZ@S�@C�@o@
�H@
��@
~�@
-@	��@	�#@	��@	X@	&�@	%@��@��@��@�9@�u@�@A�@b@b@b@  @�w@�P@|�@K�@+@
=@�y@ȴ@�R@�R@�+@V@�T@@��@�h@`B@?}@V@�@�j@�D@Z@(�@��@�m@ƨ@��@�@C�@"�@��@�\@�@�@�^@�7@X@7L@&�@ �9@  �?��w?�|�?�;d?��?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>wBC�BD�BA�B�B�B
=B��B�NB��B��B�}B�^B�?B�'B�B��B��B��B�oB�DB�B~�B�B�Bx�Br�Bq�BjBcTBS�BI�BE�BB�BA�B?}B<jB6FB1'B(�B�B{BVBB�B�sB�`B�B��B��B�wB�-B��B��B��B��B��B��B�uB�bB�7B~�Br�B`BBM�BD�B;dB/B�BJB
��B
��B
�B
�B
�B
�B
�mB
�NB
�#B
��B
ɺB
ŢB
��B
�jB
�LB
�B
��B
�JB
�+B
|�B
t�B
k�B
]/B
T�B
J�B
B�B
=qB
7LB
0!B
%�B
�B
hB
%B	��B	�B	�yB	�HB	�B	��B	��B	ɺB	��B	�jB	�RB	�9B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	�oB	�VB	�7B	�B	y�B	r�B	k�B	ffB	bNB	]/B	W
B	Q�B	L�B	G�B	C�B	=qB	6FB	.B	'�B	#�B	�B	�B	{B	PB	%B	B��B��B�B�sB�`B�TB�BB�5B�)B�#B�B�
B��B��B��B��BɺBǮBÖB�}B�wB�qB�dB�^B�^B�RB�FB�9B�'B�'B�'B�!B�B�B�B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�VB�7B�B�B� B~�B}�B|�By�Bv�Bs�Bp�Bm�Bl�Bk�BiyBhsBe`BbNB^5B\)B[#BZBW
BT�BS�BR�BQ�BP�BO�BM�BL�BJ�BI�BH�BG�BC�B?}B<jB;dB:^B9XB9XB8RB8RB8RB7LB7LB6FB5?B33B1'B/B-B,B+B+B)�B(�B&�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B%�B$�B$�B%�B#�B%�B%�B%�B&�B'�B'�B%�B'�B(�B+B-B.B.B/B/B/B.B1'B5?B8RB:^B=qB>wB>wB?}B?}B?}B@�B@�BB�BB�BD�BF�BI�BI�BJ�BK�BL�BL�BL�BM�BL�BL�BL�BN�BP�BP�BO�BO�BO�BS�BVBVBW
BXBYB\)B]/B_;B`BB`BBaHBbNBbNBbNBcTBe`BffBjBk�Bm�Bs�Bu�Bw�By�B{�B}�B� B�B�B�+B�7B�=B�JB�VB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�'B�3B�FB�^B�dB�qB��BŢBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�
B�
B�
B�B�/B�BB�HB�NB�TB�fB�sB�yB�B�B�B�B��B��B��B��B	B	B	B	+B	1B	
=B	JB	hB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	%�B	&�B	&�B	&�B	'�B	+B	.B	/B	2-B	5?B	7LB	9XB	:^B	:^B	;dB	<jB	?}B	A�B	A�B	A�B	B�B	E�B	G�B	G�B	K�B	N�B	P�B	VB	ZB	]/B	aHB	cTB	dZB	e`B	hsB	iyB	iyB	iyB	jB	n�B	o�B	p�B	r�B	v�B	|�B	~�B	� B	�B	�B	�+B	�1B	�1B	�1B	�=B	�DB	�JB	�JB	�PB	�PB	�VB	�VB	�\B	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�jB	�qB	�wB	�wB	��B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
	7B

=B
DB
DB
JB
JB
JB
PB
VB
\B
\B
\B
bB
bB
hB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
.B
/B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
;dB
;dB
;dB
;dB
<jB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
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
M�B
M�B
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
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
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
YB
YB
YB
YB
YB
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
\)B
\)B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
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
k�B
k�B
k�B
l�B
l�B
m�B
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
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>�BDBF%BFB%FB�B�B�B�B�QB�sB�SB�(B��B��B��B��B�,B�VB��B��B�GB��B��B�{Bz�Bt�BsBl"Be�BU�BJrBF%BB�BBB@�B>B7�B2�B*�BVBB�B�B�TB�B��B�	B�SB��B��B��B�|B�B��B��B�B��B�aB��B��B�oButBb�BOBBE�B=<B2BOBB
��B
��B
��B
��B
�B
�qB
�B
�B
ܬB
�aB
ʌB
�YB
�uB
��B
�XB
��B
�+B
��B
��B
~�B
v�B
m�B
^�B
V�B
K�B
C�B
>wB
8�B
2B
'�B
�B
�B
�B	�DB	�B	�B	� B	�qB	�2B	�NB	��B	��B	�qB	�>B	�%B	�|B	��B	��B	�B	��B	�=B	��B	�B	��B	��B	��B	�B	�mB	{�B	t�B	m)B	g�B	c�B	_B	X�B	SuB	N<B	H�B	EmB	?�B	8�B	/�B	)B	%FB	!bB	#B	�B	�B	�B	B	  B��B�B�*B�2B�@B��B޸BܬBیB��B��B��B� BΊB�dB��B�BĜB�4B��B�B��B��B��B��B�2B��B��B�[B�[B�oB��B��B��B��B��B�
B�HB�#B�B�1B�$B��B��B��B��B��B�.B��B�mB��B��BcB~�B}�B{JBx8BuZBq�BnIBm)Bl=Bj0Bi�BgmBdtB^�B\�B\)B[�BW�BU�BT�BS�BR�BQ�BP�BN�BN"BK^BJ=BI�BI�BFtBB'B=qB<6B:�B9�B9�B8�B8�B8�B7�B7�B6�B6�B5%B2GB0�B.�B,�B+�B+�B*�B*�B(�B'mB'�B&�B'8B'B'B'B'B'B'B'8B&fB&B&LB&�B&B'mB'8B'RB($B(�B)�B'�B(�B)�B+�B-�B.}B.}B/�B/�B0B0!B33B6�B9>B;0B>B?B>�B?�B?�B@ BA;BAoBC-BC�BESBG+BJ	BJ=BK^BLdBM6BMPBMjBNVBMjBMPBM�BO\BQ4BQ4BPHBP}BP�BT�BV�BV�BW�BX�BZB\�B]�B_�B`�B`�Ba|Bb�Bb�Bb�Bc�Be�BgBkBl=Bn�Bt9BvFBxRBz^B|�B~wB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�B�B�4B�TB�8B�DB�KB�6B�"B�"B�"B�"B�"B�qB�}B�vB��B��B��B��B��B�B��B��B��B��B��B��B��B��B�B�dB�6B�B�B�.B�&B�,B�B�B�B�9B�$B�$B�YBڠBݘB�vB�bB�B�B�B��B��B��B��B�B��B��B�B�DB�BB	UB	[B	�B	_B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&B	'B	'B	'B	(>B	+QB	.IB	/iB	2|B	5�B	7�B	9rB	:xB	:�B	;�B	<�B	?�B	A�B	A�B	A�B	B�B	E�B	G�B	HB	L0B	O(B	QhB	VmB	ZkB	]~B	abB	c�B	d�B	e�B	h�B	i�B	i�B	i�B	kB	n�B	o�B	qB	sB	w2B	}"B	HB	�OB	�GB	�MB	�EB	�KB	�KB	�fB	�XB	�^B	�dB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�B	�,B	�B	�8B	�B	�B	�QB	�CB	�IB	�/B	�OB	�AB	�AB	�MB	�TB	�tB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�HB	�B	�B	�2B	�SB	�yB	�=B	�CB	�/B	�IB	�/B	�IB	�dB	�jB	�VB	�\B	�bB	�bB	�hB	�nB	�B	�tB	�zB	�B	�B	�B	�mB	�mB	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�"B	�B	�(B	�B
 B
 4B
 B
;B
AB
aB
9B
YB
?B
?B
EB
+B
_B
EB
_B
	�B

XB
xB
^B
dB
dB
dB
�B
�B
vB
vB
vB
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!B
# B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
&B
%�B
'B
'B
($B
)B
)DB
*B
+B
+B
+6B
+B
+B
+B
,WB
-CB
-]B
.cB
/iB
1AB
1AB
2GB
2GB
2GB
2GB
2|B
3MB
4TB
5?B
5?B
5?B
5ZB
5tB
5ZB
5ZB
6`B
6`B
6`B
6`B
7fB
7�B
7fB
8�B
8lB
8�B
8lB
8lB
8�B
9�B
;�B
;B
;B
;�B
<�B
=�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
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
MB
M�B
M�B
N�B
OB
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
QB
RB
RB
RB
SB
SB
S�B
TB
TB
TB
TB
TB
T�B
UB
UB
UB
T�B
VB
UB
UB
VB
VB
VB
W$B
W$B
W?B
W?B
W?B
X+B
YB
YB
Y1B
Y1B
YKB
Y1B
Y1B
Z7B
Z7B
ZB
Z7B
Z7B
ZQB
[=B
[WB
[=B
\xB
\]B
]dB
^jB
^�B
^jB
_VB
_VB
_VB
`vB
`\B
`vB
`BB
`BB
`\B
abB
abB
bhB
b�B
cnB
cnB
cnB
cnB
dtB
dtB
e`B
ezB
ezB
ezB
ezB
ezB
ezB
f�B
f�B
f�B
f�B
f�B
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
h�B
h�B
hsB
h�B
iyB
i�B
i�B
i�B
iyB
iyB
i�B
iyB
iyB
i�B
i�B
jB
jB
jB
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
m�B
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
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
w�B
y	B
y�B
y�B
zB
zB
zB
z�B
z*B
{B
|B
|B
|B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.04(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006280031492020062800314920200628003149202211182143292022111821432920221118214329202006290017532020062900175320200629001753  JA  ARFMdecpA19c                                                                20200617003812  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200616153935  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200616153938  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200616153938  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200616153939  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200616153939  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200616153939  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200616153939  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200616153939  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200616153939  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200616153939  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200616153939                      G�O�G�O�G�O�                JA  ARUP                                                                        20200616155328                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200616153443  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200616153420  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20200616153420  CV  JULD_LOCATION   G�O�G�O�F�                JM  ARGQJMQC2.0                                                                 20200616153420  CV  LONGITUDE       G�O�G�O��$�F                JM  ARCAJMQC2.0                                                                 20200627153149  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200627153149  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200628151753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124329  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                