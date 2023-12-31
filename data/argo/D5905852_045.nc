CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-28T06:48:35Z creation;2020-04-28T06:48:37Z conversion to V3.1;2022-08-02T05:11:05Z update;     
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
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200428064835  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  A30_8420_045                    2C  D   APEX                            8420                            2.11.2                          846 @�B����1   @�C3�� @0^�m\���c(��+1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   CL�C�fC�fC  C	�fC  C  C  C  C�C�fC�fC  C�C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6L�C7��C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX33CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@
�H@u�@���@���AQ�A<��A\z�A|z�A�ffA�Q�A��\A���AΣ�A���A�z�A�=qB(�B  B33B=qB'=qB/33B7(�B?33BG(�BO�BW33B_33Bg(�Bo=qBw=qB33B���B���B���B��B�.B�  B���B���B��{B���B���B��\B��=B���B���B��{Bó3B��fBˮBϳ3Bӊ=B�z�B�z�Bߔ{B㙚B癚B뙚BB�B��=B��\B��C#�C��C�RC�C	�RCǮC�=C�=C�\C޸C��C�qC�\C�fC�C��C!C#�C%ǮC'��C)�\C+��C-��C/��C1�=C3�\C6�C7�
C9�3C;ǮC=�\C?�CA�=CCǮCECGǮCI�=CK�\CM��CO��CQ��CS��CU�=CW�RCY��C[��C]��C_��Ca�{Cc�{Ce�{Cg��Ci�\Ck�=Cm��Co�\Cq�\Cs�Cu�=Cw��Cy��C{�=C}�\C��C���C���C��C��fC��C��=C��C��=C��C��fC��=C���C��fC��=C��C��C��=C��fC��fC��=C��C���C��fC��=C��C��C��C��C��C��=C��C�� C��C��C��C��C��C��C��C��C��fC��fC���C��C��HC��C��fC��C��fC��C��C��C��C��C��C��C��C��C��fC���C���C��C��HC��C��C��fC���C��C��C���C���C��C��C��C��C���C��C��C��fC��fC��fC���C��C���C���C��C��C���C��=C��=C��C��C��C��C��C��fC��fC��fC��C��C��C��C��C��C��C��C���C��fC���C��C��HC���C��=C��fC��C�� C��C��fC��C���C��C��HC��C���C��C��C��C��D s�D ��Dq�D�3Ds�D�{Ds�D��Ds�D�Ds3D�3Dr�D�3Dt{D��Dq�D�3D	r�D	�HD
r�D
��DuD��Dp�D��Ds�D��Dq�D�DqHD�Dq�D��Ds3D�HDr�D�{Ds3D��Dq�D�Ds3D�Ds�D�3DqHD�Ds�D��Dq�D��Dp�D�3Dt{D��DqHD�Du�D�{Dq�D�3DuD�D o\D � D!qHD!�HD"r�D"��D#t{D#��D$s3D$��D%s3D%�{D&s�D&�3D'u�D'��D(s3D(�3D)s3D)��D*p D*��D+t{D+�D,s�D,�fD-vfD-��D.r�D.�{D/s�D/�3D0s�D0��D1uD1�{D2q�D2�3D3s�D3�D4qHD4�D5uD5�{D6t{D6�{D7qHD7�HD8q�D8�3D9s�D9�HD:p�D:�D;r�D;��D<r�D<�D=uD=�{D>t{D>��D?qHD?�3D@s3D@��DAs3DA�DBq�DB�DCt{DC�3DDs3DD�HDEqHDE�3DFq�DF�DGqHDG�DHuDH��DIs�DI��DJp�DJ�HDKr�DK�3DLs�DL��DMuDM��DNu�DN�3DOs3DO�3DPs�DP��DQt{DQ��DRt{DR��DSt{DS��DTs3DT��DUp�DU�HDVt{DV�fDWuDW�DXt{DX�3DYs3DY�{DZuDZ��D[t{D[��D\r�D\�HD]r�D]��D^r�D^��D_s�D_�D`qHD`�HDar�Da��Dbs3Db�{Dcu�Dc�Ddr�Dd�3Det{De��DfuDf�{Dgr�Dg�Dhr�Dh�{Dis�Di��DjqHDj��Dks3Dk��Dlq�Dl�3Dms3Dm�HDnp�Dn��Dot{Do��Dps3Dp�{Dqs3Dq�3Drs3Dr��Dsq�Ds�Dtt{Dt��DuvfDu�Dvq�Dv�Dwr�Dw�Dxs3Dx�3Dys3Dy�3Dzq�Dz��D{qHD{�D|t{D|�{D}s3D}�D~s�D~�Dt{D�3D�8�D�yHD���D��=D�:=D�y�D��RD���D�8�D�x�D���D��HD�9�D�y�D���D��=D�9�D�y�D���D���D�9�D�z�D��=D��=D�9�D�y�D���D���D�9�D�yHD���D���D�9HD�yHD���D���D�:�D�z�D���D���D�8�D�y�D���D���D�9�D�yHD��=D���D�:�D�y�D���D��=D�9�D�z=D���D���D�9HD�yHD���D���D�9�D�z=D���D���D�:=D�y�D���D�� D�9�D�z�D��=D��=D�:�D�z=D���D���D�9�D�x�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9HD�y�D��=D���D�9�D�y�D���D���D�9HD�z=D���D���D�9�D�yHD���D���D�9HD�z=D���D��HD�9�D�x�D���D���D�8�D�yHD��HD��HD�9�D�z=D��HD��HD�9HD�y�D��HD���D�9�D�y�D���D���D�8RD�yHD���D���D�8�D�z�D��3D��=D�9HD�x�D���D��HD�:=D�z�D���D���D�9HD�yHD���D���D�9�D�y�D���D���D�9�D�yHD���D���D�:�D�z=D���D��HD�9HD�y�D���D���D�:�D�y�D���D���D�8�D�xRD���D���D�9�D�x�D��HD���D�:=D�z=D���D���D�:=D�z�D���D���D�;3D�x�D���D���D�9HD�x�D��HD��HD�9�D�y�D��HD���D�8�D�yHD��=D���D�9�D�yHD��=D���D�9�D�yHD���D��HD�8�D�yHD���D���D�9�D�y�D��HD��=D�9HD�y�D���D��HD�8�D�xRD���D���D�:=D�y�D���D���D�:=D�z=D���D���D�9�D�yHD�� D���D�8�D�y�D���D��HD�9�D�x�D��HD���D�9HD�x�D���D��HD�9�D�z=D���D��RD�8�D�y�D���D���D�9�D�xRD���D��HD�9�D�y�D���D���D�9�D�y�Dº=D���D�:�D�z�Dú=D��=D�:�D�z�Dĺ�D��3D�;3D�y�DŸ�D���D�8�D�xRDƹHD��HD�8�D�z=DǺ=D���D�9HD�yHDȹHD���D�9�D�y�Dɸ�D��HD�:=D�y�Dʹ�D��HD�9�D�z=D˺=D���D�8�D�x�D̸RD���D�:�D�yHD͸�D���D�:=D�z�Dι�D���D�9�D�z=DϺ=D���D�9�D�x�Dй�D��=D�9�D�y�Dѹ�D���D�8�D�y�Dҹ�D���D�:=D�x�DӹHD���D�:=D�y�DԹ�D��=D�8�D�yHDչHD��HD�9HD�y�Dֺ�D���D�8RD�x�D׹�D���D�9�D�z=Dع�D���D�9�D�z=Dٺ=D��HD�8�D�x�Dڸ�D��HD�:�D�y�D۹�D��=D�9�D�x�Dܸ�D���D�:=D�{3Dݺ�D���D�9�D�y�D޹�D��=D�9�D�y�D߸�D��RD�9HD�x�D�RD��HD�9�D�y�DṚD���D�8�D�z=D��D��HD�9HD�x�D㸤D���D�9�D�y�D乚D���D�8�D�x�D�HD��HD�9HD�y�D��D���D�8�D�x�D�HD���D�9�D�y�D�HD���D�:=D�z=D�=D���D�9HD�yHD긤D��RD�9�D�y�D��D��=D�9�D�y�D��D���D�9HD�yHD���D��HD�9�D�x�D��D���D�9�D�yHD��D���D�9�D�y�D�=D���D�:�D�z�D�D���D�9�D�yHD�D�� D�8�D�x�D��D��=D�9�D�x�D���D���D�9�D�y�D���D��=D�:�D�z�D���D���D�8�D�x�D��=D���D�9�D�yHD���D���D�8RD�x�D���D���D�9�D�x�D���D��=D�9�D�x�D��RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�FA�GEA�E9A�A�A�D�A�JXA�MA�P�A�TaA�OBA�J�A�IA�H�A�K^A�K^A�K�A�M�A�I�A�G�A�C-A�9�A�1�A�2�A�2-A�.�A�.}A�.�A�/�A�1[A�+kA�$tA�"hA��A��A��A���A��)A�?}A�P�A�oiA���A��	A��A��JA�)_A���A��xA�N�A��A��RA���A���A�{JA�OvA�C�A�;dA�1'A���A��<A��A��NA�+�A��5A��'A���A�J#A�+�A��pA��iA��(A���A�XyA��$A���A�ѷA��LA��4A�7A�FA�ʌA��2A��4A���A���A�ޞA��A��	A�	�A���A���A~V�A|h
Ax!-Ap.�AkAj7LAgZAduA`ԕA\�AXzxAU.IAS��AQ��AOe�AMo AJ�3AHw2AF��AEc ACOvAB�PAA�AAZ�A@��A?�"A>v�A=zxA<�^A<#�A;��A;��A9��A8�#A8��A8,�A6��A5h�A3�9A1��A1OvA13�A0�/A/�A,�ZA,��A,+A+��A+w2A+^�A*�DA*�fA+qA+U�A+w2A+�YA*ߤA)�tA'�bA&�IA$��A$eA$	lA#�HA#[�A"�BA"4A!ںA!o A �;A a�A *�A��A��A�A��A�KA"hA�7AJ#A�wAZ�A�A�AZA3�A�hAqA�hA�A�#A��AC�A֡A=qA4A�A�AA��AC-A�A��A)_AoA��A{�A.IA�>A{�A[WA>�A~A�A� AY�A/A��A��ARTA��A��A��AS&A�yAh
A#�A�OAo A"�A
��A
��A
��A
a�A
@OA
A	��A	d�A	GEA	�AԕA|�A;�A�AA��Au%A��A�4AY�AFA;A�VA��A��A��Ae�A�2A��A�A�DA��Aw�AZ�A ��A �A jA 0�A �@��V@�33@���@��@���@��@��@��@�y>@���@���@�x@��@�tT@���@�"�@��]@�v`@�L0@���@�V@��@﫟@���@�H�@�B�@쒣@��@���@�@�p;@��@�@�M�@�~@��T@��N@��@��@䲖@�˒@��?@�@��@�[W@�c @��@���@ݗ$@�J�@��@��@�Ĝ@ܧ@�n/@���@گO@ھ@�֡@�͟@ڨ�@�1@�=�@�S@�~�@�L0@�_�@�a|@ײ-@���@��@�	@�Z�@Һ�@�N�@њk@�=@�,=@ϵt@�`B@��@�j@���@�U�@�:�@���@�֡@��E@̻�@��@ˈf@��@�^5@�ƨ@ɮ�@ɠ'@ɏ�@�u�@��M@ȇ�@��@�0�@� i@Ƨ@��@��T@Ţ�@��s@�c @�K^@��6@���@�YK@�@�@�\�@���@��@�3�@�j@�bN@�L�@�2�@��z@��@��>@��K@�"h@�q@�u%@�B[@���@�e�@�ں@��<@��_@�w�@�Ta@���@���@�t�@�^�@�O�@��@�J�@�˒@��f@�/@���@�ff@�PH@�>B@��@�F�@��[@���@���@�B[@���@�l�@���@���@��8@��)@�@�@�#:@�($@�.�@��@�X�@�ں@��o@��Z@��o@���@�qv@��@�?�@���@��5@���@�|�@�W�@���@���@�\�@��@���@�ff@��@��@@��~@�o @��@���@��@���@�1�@���@�H�@�($@��@��@�4�@�ں@���@�J�@��@���@�j@�a@�L�@�/@�%@�bN@�� @�m]@�X@�1�@�ں@���@�g8@�l"@�:*@��4@��@���@���@��D@��@��"@���@�L�@��@��/@��@���@��k@�j�@�L�@�*0@���@���@�g8@��@��g@��@�hs@� \@��5@��B@���@�C-@��)@��@���@���@�2a@��v@��@���@���@�{�@�_@���@�O�@�1�@��@��e@�!@���@�@��*@�A�@��@��u@���@��@��o@�ݘ@��'@�t�@�H�@�1�@�+�@�(�@��H@��O@��@��A@�e�@�;�@��@���@���@��P@�o @�.I@��"@��@��F@�Q�@�M@�D�@��@� �@���@��h@��@�v�@�/�@��}@���@���@�7L@��@��I@�Q�@��#@��f@�|�@�J#@� i@�[�@��>@��;@���@��*@�X�@���@�i�@�/�@�0U@�	@���@��N@���@��f@�G�@�%F@��@�C-@�خ@�o @��@���@�?@�7@ݘ@��@C�@&@~��@~M�@}�j@}a�@}0�@|�P@|��@|c�@|:�@{��@{.I@z�2@zxl@z.�@y�-@yG�@x�5@x��@x!@x�@w��@w�@@w�@@w��@wX�@w�@w i@v��@v��@u��@u%@t�/@t�e@t��@t[�@tS�@t'R@s�	@sK�@r͟@r� @rC�@q�>@qs�@p�p@p�u@ptT@pM@o�@o��@o�F@o�[@o��@o��@o�	@oj�@oX�@o�@oo@n��@ni�@n1�@m��@l]d@k��@k��@ka@j:*@i�h@i/@h�@h�_@g��@g�@f�y@f��@f5?@e�j@e}�@eJ�@e�@d�@c�a@cx@cZ�@c4�@b�}@bM�@b3�@a��@a�@a:�@`~(@`1'@` �@`x@_�r@_�@_˒@_��@_�@@_�4@_=@^�@^��@]�-@]5�@](�@\��@\�@\��@\1'@\�@[�[@[J#@[�@Zi�@Yԕ@Ya�@Y�@X�	@X�e@XM@X4n@X7@W�Q@We�@W/�@V��@Vp;@Up�@U&�@T�P@T�@T�p@TQ�@S��@S��@S�@Sl�@S\)@SJ#@S�@R�@R�2@R�L@R�@Q��@Q2a@Qq@Q@Q4@P�_@P�@O�&@O��@O/�@O
=@N�L@N~�@NYK@N:*@N6�@N	@Mc@L�@LV�@K�;@K{J@KA�@K$t@J�@J��@Jh
@JR�@J�@I��@Ic�@I5�@I�@H�p@Hz�@G�K@F��@Fa|@F{@E�M@EDg@EV@D�O@DV�@D@C��@B��@B{@A�T@A�N@A�H@A��@Ap�@AT�@A�@@�@@��@@*�@?�m@?�V@?t�@?RT@?�@>͟@>Ov@>	@=�@=��@=��@=e,@=5�@<�|@<��@<V�@<�@;�@;��@;b�@:͟@:Ov@9�@9�z@9��@9S&@9�@8�|@8��@8N�@8�@7�@7��@7�A@7�;@7RT@6��@6{�@6{@5��@5�@5�T@5�T@5�3@5B�@4��@4��@4h�@4 �@3�@3dZ@36z@2��@2u%@2=q@1�@1��@1��@1?}@0�@0��@0e�@0A�@0-�@0"h@0G@/��@/��@/��@/v`@/g�@/S�@.��@.�@.�@-��@-�=@-rG@-4@,�@,[�@,�@+خ@+�@+A�@*�s@*xl@*O@)��@)ԕ@)�t@)}�@)hs@):�@)�@(Ɇ@(��@(1@'�[@'~�@'j�@'\)@'U�@'A�@'"�@' i@&�@&��@&i�@&3�@%��@%�"@%/@%V@$��@$��@$��@$�Y@$_@$Q�@$A�@$/�@#�Q@#�0@#��@"��@"q�@"5?@")�@!��@!��@!c�@!*0@ �@ ��@ ��@ m�@ D�@ 2�@ 1@˒@��@�:@t�@O@;d@��@h
@_@��@�=@o @Q�@!�@;@�@��@�[@��@�@m�@N�@7@��@]�@E9@"�@o@��@M�@�C@�'@��@!�@�@�9@�)@�p@Ɇ@�o@N�@�@�Q@�@l�@J#@Y@�2@J�@u@�@��@��@�h@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�FA�GEA�E9A�A�A�D�A�JXA�MA�P�A�TaA�OBA�J�A�IA�H�A�K^A�K^A�K�A�M�A�I�A�G�A�C-A�9�A�1�A�2�A�2-A�.�A�.}A�.�A�/�A�1[A�+kA�$tA�"hA��A��A��A���A��)A�?}A�P�A�oiA���A��	A��A��JA�)_A���A��xA�N�A��A��RA���A���A�{JA�OvA�C�A�;dA�1'A���A��<A��A��NA�+�A��5A��'A���A�J#A�+�A��pA��iA��(A���A�XyA��$A���A�ѷA��LA��4A�7A�FA�ʌA��2A��4A���A���A�ޞA��A��	A�	�A���A���A~V�A|h
Ax!-Ap.�AkAj7LAgZAduA`ԕA\�AXzxAU.IAS��AQ��AOe�AMo AJ�3AHw2AF��AEc ACOvAB�PAA�AAZ�A@��A?�"A>v�A=zxA<�^A<#�A;��A;��A9��A8�#A8��A8,�A6��A5h�A3�9A1��A1OvA13�A0�/A/�A,�ZA,��A,+A+��A+w2A+^�A*�DA*�fA+qA+U�A+w2A+�YA*ߤA)�tA'�bA&�IA$��A$eA$	lA#�HA#[�A"�BA"4A!ںA!o A �;A a�A *�A��A��A�A��A�KA"hA�7AJ#A�wAZ�A�A�AZA3�A�hAqA�hA�A�#A��AC�A֡A=qA4A�A�AA��AC-A�A��A)_AoA��A{�A.IA�>A{�A[WA>�A~A�A� AY�A/A��A��ARTA��A��A��AS&A�yAh
A#�A�OAo A"�A
��A
��A
��A
a�A
@OA
A	��A	d�A	GEA	�AԕA|�A;�A�AA��Au%A��A�4AY�AFA;A�VA��A��A��Ae�A�2A��A�A�DA��Aw�AZ�A ��A �A jA 0�A �@��V@�33@���@��@���@��@��@��@�y>@���@���@�x@��@�tT@���@�"�@��]@�v`@�L0@���@�V@��@﫟@���@�H�@�B�@쒣@��@���@�@�p;@��@�@�M�@�~@��T@��N@��@��@䲖@�˒@��?@�@��@�[W@�c @��@���@ݗ$@�J�@��@��@�Ĝ@ܧ@�n/@���@گO@ھ@�֡@�͟@ڨ�@�1@�=�@�S@�~�@�L0@�_�@�a|@ײ-@���@��@�	@�Z�@Һ�@�N�@њk@�=@�,=@ϵt@�`B@��@�j@���@�U�@�:�@���@�֡@��E@̻�@��@ˈf@��@�^5@�ƨ@ɮ�@ɠ'@ɏ�@�u�@��M@ȇ�@��@�0�@� i@Ƨ@��@��T@Ţ�@��s@�c @�K^@��6@���@�YK@�@�@�\�@���@��@�3�@�j@�bN@�L�@�2�@��z@��@��>@��K@�"h@�q@�u%@�B[@���@�e�@�ں@��<@��_@�w�@�Ta@���@���@�t�@�^�@�O�@��@�J�@�˒@��f@�/@���@�ff@�PH@�>B@��@�F�@��[@���@���@�B[@���@�l�@���@���@��8@��)@�@�@�#:@�($@�.�@��@�X�@�ں@��o@��Z@��o@���@�qv@��@�?�@���@��5@���@�|�@�W�@���@���@�\�@��@���@�ff@��@��@@��~@�o @��@���@��@���@�1�@���@�H�@�($@��@��@�4�@�ں@���@�J�@��@���@�j@�a@�L�@�/@�%@�bN@�� @�m]@�X@�1�@�ں@���@�g8@�l"@�:*@��4@��@���@���@��D@��@��"@���@�L�@��@��/@��@���@��k@�j�@�L�@�*0@���@���@�g8@��@��g@��@�hs@� \@��5@��B@���@�C-@��)@��@���@���@�2a@��v@��@���@���@�{�@�_@���@�O�@�1�@��@��e@�!@���@�@��*@�A�@��@��u@���@��@��o@�ݘ@��'@�t�@�H�@�1�@�+�@�(�@��H@��O@��@��A@�e�@�;�@��@���@���@��P@�o @�.I@��"@��@��F@�Q�@�M@�D�@��@� �@���@��h@��@�v�@�/�@��}@���@���@�7L@��@��I@�Q�@��#@��f@�|�@�J#@� i@�[�@��>@��;@���@��*@�X�@���@�i�@�/�@�0U@�	@���@��N@���@��f@�G�@�%F@��@�C-@�خ@�o @��@���@�?@�7@ݘ@��@C�@&@~��@~M�@}�j@}a�@}0�@|�P@|��@|c�@|:�@{��@{.I@z�2@zxl@z.�@y�-@yG�@x�5@x��@x!@x�@w��@w�@@w�@@w��@wX�@w�@w i@v��@v��@u��@u%@t�/@t�e@t��@t[�@tS�@t'R@s�	@sK�@r͟@r� @rC�@q�>@qs�@p�p@p�u@ptT@pM@o�@o��@o�F@o�[@o��@o��@o�	@oj�@oX�@o�@oo@n��@ni�@n1�@m��@l]d@k��@k��@ka@j:*@i�h@i/@h�@h�_@g��@g�@f�y@f��@f5?@e�j@e}�@eJ�@e�@d�@c�a@cx@cZ�@c4�@b�}@bM�@b3�@a��@a�@a:�@`~(@`1'@` �@`x@_�r@_�@_˒@_��@_�@@_�4@_=@^�@^��@]�-@]5�@](�@\��@\�@\��@\1'@\�@[�[@[J#@[�@Zi�@Yԕ@Ya�@Y�@X�	@X�e@XM@X4n@X7@W�Q@We�@W/�@V��@Vp;@Up�@U&�@T�P@T�@T�p@TQ�@S��@S��@S�@Sl�@S\)@SJ#@S�@R�@R�2@R�L@R�@Q��@Q2a@Qq@Q@Q4@P�_@P�@O�&@O��@O/�@O
=@N�L@N~�@NYK@N:*@N6�@N	@Mc@L�@LV�@K�;@K{J@KA�@K$t@J�@J��@Jh
@JR�@J�@I��@Ic�@I5�@I�@H�p@Hz�@G�K@F��@Fa|@F{@E�M@EDg@EV@D�O@DV�@D@C��@B��@B{@A�T@A�N@A�H@A��@Ap�@AT�@A�@@�@@��@@*�@?�m@?�V@?t�@?RT@?�@>͟@>Ov@>	@=�@=��@=��@=e,@=5�@<�|@<��@<V�@<�@;�@;��@;b�@:͟@:Ov@9�@9�z@9��@9S&@9�@8�|@8��@8N�@8�@7�@7��@7�A@7�;@7RT@6��@6{�@6{@5��@5�@5�T@5�T@5�3@5B�@4��@4��@4h�@4 �@3�@3dZ@36z@2��@2u%@2=q@1�@1��@1��@1?}@0�@0��@0e�@0A�@0-�@0"h@0G@/��@/��@/��@/v`@/g�@/S�@.��@.�@.�@-��@-�=@-rG@-4@,�@,[�@,�@+خ@+�@+A�@*�s@*xl@*O@)��@)ԕ@)�t@)}�@)hs@):�@)�@(Ɇ@(��@(1@'�[@'~�@'j�@'\)@'U�@'A�@'"�@' i@&�@&��@&i�@&3�@%��@%�"@%/@%V@$��@$��@$��@$�Y@$_@$Q�@$A�@$/�@#�Q@#�0@#��@"��@"q�@"5?@")�@!��@!��@!c�@!*0@ �@ ��@ ��@ m�@ D�@ 2�@ 1@˒@��@�:@t�@O@;d@��@h
@_@��@�=@o @Q�@!�@;@�@��@�[@��@�@m�@N�@7@��@]�@E9@"�@o@��@M�@�C@�'@��@!�@�@�9@�)@�p@Ɇ@�o@N�@�@�Q@�@l�@J#@Y@�2@J�@u@�@��@��@�h@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BĶBĶB��BðB��BāB�B�B��B��B��BðB��BðBðBðB��BðB�aB�B��B��B��B��BB�{B�aBÖB�{BðB��BªB�AB��B�iB�JB�B�B	BAB	^jB	]dB	��B	��B	��B	҉B	�ZB
B
fB
pB
2B
�B
"B
%�B
-B
9�B
^jB
��B
�B
�KB
��B
�B B!B~BFB	�B�B�BB�B
��B
�)B
�9B
�SB
�*B
� B
�TB
�B
��B
raB
X�B
T�B
@B
4�B
$B
�B	�fB	�B	�gB	�{B	�B	�8B	��B	h�B	RoB	M�B	E�B	8lB	*B	�B	B	 iB��B�"B	9B	<�B	ESB	y�B	�B	��B	�BB	�$B	�GB	��B	ĜB	уB	�B	�B	�B	�$B

�B
"NB
&�B
,WB
-]B
0oB
:^B
?.B
A�B
5?B
/�B
.cB
*0B
�B
:B
7B
�B
�B
B
�B
�B
"NB
9�B
8�B
=�B
DB
@�B
7�B
)yB
�B
�B
sB
yB
�B
�B
#nB
$ZB
'�B
+QB
+�B
)�B
(�B
($B
)B
&2B
"�B
 \B
�B
	B
#B
'B
'mB
)�B
.B
.cB
.�B
.�B
.�B
.B
+kB
.�B
1vB
3B
5?B
4�B
49B
3�B
4nB
4�B
1vB
0�B
/ B
.�B
1vB
2�B
3B
3�B
49B
3�B
8RB
8�B
8lB
8B
6�B
6B
6�B
8B
9XB
9XB
8�B
7�B
7�B
7�B
8lB
9>B
9XB
88B
<B
<6B
<jB
<6B
<B
;�B
;dB
<B
<�B
<PB
<6B
;�B
;�B
;�B
:�B
:^B
:*B
:^B
:xB
9$B
8�B
8RB
8B
7�B
4�B
3�B
4B
3�B
2aB
.�B
+kB
%�B
#TB
"�B
"�B
"�B
!�B
!B
 �B
#:B
&B
&�B
%�B
%FB
"�B
#nB
$�B
"B
VB
xB
�B
sB
B
2B
�B
�B
�B
}B
B
�B
�B

	B
�B
mB
�B
�B
GB
'B
 B	��B	��B	��B	��B	�LB	��B	��B	��B	�B	�DB	�rB	��B	��B	�B	�CB	�B	��B	�B	�B	�B	��B	�B	�aB	�B	��B	��B	��B	��B	�B	�6B
oB
	B
KB
�B
mB
MB
gB
B
YB
B
�B
�B
 OB	�2B	��B	�B	�B	�AB	�B	�IB	�B	��B	��B	�B	�B	�"B	�"B	�OB	�[B	�B	�oB	��B	��B	�/B	�B	�OB	�OB	�OB	�OB	�B	�!B	�;B	��B	�!B	�;B	��B	�!B	�B	�B	� B	�B	��B	�"B	�WB	�!B	�B	�]B	�=B	�
B	��B	�B	�B	�eB	��B	�wB	�B	�B	�}B	��B	��B	��B	�nB	��B	��B	��B	�B	��B	�9B	�B	�hB	��B	��B	��B	��B	�`B	��B	��B	��B	��B	�8B	�RB	�lB	��B	��B	�rB	�>B	��B	��B	�B	�xB	�0B	�dB	�}B
�B
GB
�B
�B
�B
MB
B
�B
9B
�B
3B
MB
mB
YB
�B
EB
tB
�B
�B
�B
�B
_B
	�B

XB

=B
dB
<B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
B
�B
{B
�B
B
�B
�B
�B
�B
�B
�B
�B
MB
B
�B
�B
MB
�B
mB
+B
�B
yB
_B
B
�B
�B
�B
_B
�B
�B
7B
�B
B
�B
�B
yB
�B
KB
1B
eB
B
B
QB
kB
�B
�B
�B
)B
�B
�B
�B
�B
B
�B
B
=B
#B
qB
�B
�B
�B
�B
=B
)B
�B
�B
�B
�B
B
�B
�B
B
�B
B
�B
�B
�B
B
!B
;B
;B
 �B
 �B
�B
 vB
"�B
#nB
#�B
#�B
#�B
#�B
#nB
#TB
#nB
#TB
#�B
#TB
#:B
#�B
$&B
$&B
%B
$tB
$�B
&�B
&�B
&�B
&�B
'8B
(
B
(sB
(>B
'�B
(>B
)B
(�B
)�B
)�B
)DB
)�B
)_B
)�B
,WB
.IB
./B
,�B
,=B
,qB
/iB
/�B
/iB
0B
0;B
/�B
/�B
/�B
/OB
.�B
/ B
/iB
0!B
/�B
/�B
/iB
0B
0;B
0oB
0�B
0�B
1�B
2�B
2GB
1�B
1�B
1�B
1�B
2-B
3�B
4B
4nB
4�B
5%B
5?B
5%B
5B
5ZB
5�B
5�B
6B
6+B
6B
6FB
6�B
6�B
6�B
6�B
6�B
7�B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
88B
8RB
8�B
8�B
9>B
9�B
9�B
:DB
:^B
:xB
;0B
;�B
<B
=B
="B
>(B
=�B
=�B
>(B
?B
?�B
?�B
?�B
@4B
@ B
@ B
?cB
>�B
>�B
?cB
>�B
>�B
>�B
>�B
?B
?�B
?�B
?�B
@B
@iB
@�B
@�B
@�B
AB
A�B
BAB
B�B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
D�B
D�B
EB
EB
E9B
EmB
F�B
G�B
G�B
HKB
HfB
H�B
HfB
H�B
IB
IlB
I�B
I�B
I�B
I�B
JXB
J�B
JXB
JXB
J=B
JrB
J�B
KxB
K�B
LJB
LJB
LdB
L�B
MjB
M6B
MB
LdB
LB
K�B
K�B
L~B
M6B
L�B
L�B
L�B
L�B
L�B
MB
M�B
N<B
N<B
NB
NB
M�B
MPB
M6B
MPB
M�B
OB
O�B
OvB
PB
P}B
P�B
Q B
Q�B
QhB
Q�B
Q�B
QhB
QB
Q�B
R B
R�B
T{B
T�B
UB
T�B
T�B
U2B
U�B
U�B
U�B
VSB
VSB
VSB
V�B
V�B
V�B
W
B
W?B
V�B
W?B
W?B
W$B
W?B
WsB
W�B
W�B
X+B
X�B
YB
Y�B
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
Z�B
[#B
\)B
\CB
\xB
\xB
\�B
\�B
\�B
]/B
]IB
]�B
^B
^5B
^B
]�B
]�B
]�B
^B
^OB
^�B
^OB
^jB
^�B
_B
_;B
_VB
_;B
_;B
_�B
_�B
_�B
`B
`'B
`\B
`\B
`\B
`\B
`�B
abB
a�B
a�B
a�B
bB
a�B
a�B
a�B
b�B
b�B
c B
cnB
c�B
c�B
dB
dB
dZB
d�B
d�B
d�B
d�B
e,B
e`B
ezB
e�B
e�B
e�B
fB
fB
f2B
f�B
f�B
f�B
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
hsB
h�B
h�B
h�B
h�B
i*B
iyB
i�B
j0B
jKB
jeB
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mCB
m]B
m�B
nB
ncB
ncB
n�B
n�B
n�B
n�B
oB
o B
oB
o B
oOB
o5B
oiB
o�B
pUB
poB
poB
p�B
p�B
qB
qB
q[B
q�B
q�B
q�B
q�B
q�B
rB
rGB
rGB
rGB
r|B
raB
raB
r�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tnB
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
u�B
u�B
u?B
uB
u?B
v+B
u�B
u�B
u�B
vzB
wLB
w�B
xB
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y>B
zB
z*B
z^B
z^B
zxB
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�B��B�3BĶB�SB�SB�B�B��B��B��B��B��B��B��B��BÖB�GB�B�B��B�BªBðB�{BðBðB��B�-B��B��B�B��BȚB��B�;B	C�B	_VB	^�B	��B	��B	��B	�uB	��B
�B
	B
B
�B
�B
"�B
&2B
-CB
:*B
^�B
�NB
��B
�B
��B
�(BB#�B!�BB�BB�B!BNB
�dB
�B
چB
�dB
�VB
��B
��B
�B
�B
z�B
_�B
YKB
CGB
9XB
)B
�B	�JB	�B	��B	�EB	��B	�)B	��B	m�B	T�B	Q�B	J#B	<�B	/�B	pB	�B	�B	 �B	 iB	KB	?�B	HKB	|6B	�B	��B	�-B	�B	�B	��B	�B	�@B	�eB	�/B	��B	��B
�B
$@B
($B
,�B
.}B
2|B
<PB
AoB
DB
5�B
0;B
/�B
,�B
!�B
B
	B
VB
 B
OB
dB
�B
"B
9�B
8�B
>BB
E9B
B�B
:B
+6B
!�B
�B
�B
�B
�B
�B
$@B
%B
(�B
,"B
,�B
*B
)�B
)�B
*0B
'B
#�B
!bB
�B
�B
#�B
'�B
'�B
*B
.�B
.�B
/iB
/�B
/OB
/ B
+�B
/OB
1�B
3�B
5�B
5B
4�B
49B
5ZB
5tB
2B
1[B
/�B
.�B
1�B
3MB
3�B
4B
4�B
49B
8�B
8�B
8�B
8�B
6�B
6zB
72B
8�B
9�B
9�B
8�B
8B
8RB
8RB
9$B
9�B
9�B
8�B
<�B
<�B
<�B
<�B
<jB
<6B
;�B
<�B
=B
<�B
<�B
<jB
<jB
<B
;dB
:�B
:�B
;B
:�B
9XB
9	B
8�B
8�B
8�B
5?B
4B
4�B
4TB
2�B
/�B
,�B
&B
#�B
#B
#TB
# B
"B
!|B
!bB
#�B
&�B
'RB
&B
%�B
#TB
$B
%�B
"�B
 B
IB
�B
+B
�B
�B
B
,B
�B
NB
pB
6B
JB

�B
�B
B
tB
gB
�B
B
B	��B	�^B	�XB	�>B	��B	�8B	��B	��B	��B	��B	�DB	��B	��B	�B	��B	��B	�!B	�B	��B	�B	�'B	��B	�B	�B	��B	�`B	��B	��B	�*B	�jB
�B
	�B
�B
?B
�B
�B
�B
YB
�B
�B
aB
�B
�B	��B	�hB	�AB	��B	��B	�B	�B	�B	�wB	�]B	�B	��B	�qB	�WB	�iB	�B	�B	��B	�OB	�}B	�B	�OB	�B	�B	�B	��B	��B	�B	�B	�;B	��B	�B	��B	��B	�;B	�oB	�OB	�B	�B	�B	��B	��B	�;B	��B	�CB	�>B	�B	��B	�qB	��B	�B	�B	�MB	�'B	�}B	��B	�B	�9B	��B	�tB	��B	�B	�ZB	�%B	��B	��B	�B	��B	��B	�FB	�B	��B	�B	�`B	�lB	�B	�lB	��B	��B	�$B	��B	��B	��B	��B	��B	��B	��B	�dB	�dB	��B
�B
{B
�B
-B
�B
�B
mB
9B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
+B
EB
_B
�B

	B

�B

�B
�B
�B
�B
B
B
\B
 B
4B
4B
4B
TB
�B
�B
:B
B
,B
aB
,B
�B
�B
MB
2B
�B
B
B
MB
9B
B
�B
9B
�B
9B
�B
B
�B
�B
eB
�B
�B
_B
�B
�B
B
�B
�B
B
�B
�B
yB
�B
�B
�B
eB
B
B
�B
�B
�B
�B
�B
�B
	B
)B
�B
CB
�B
B
)B
CB
CB
CB
qB
=B
�B
�B
#B
#B
�B
�B
�B
CB
�B
�B
�B
xB
�B
/B
IB
B
jB
B
;B
!B
;B
;B
pB
pB
!HB
 �B
 'B
 �B
# B
#�B
$&B
#�B
$B
#�B
#�B
#�B
#�B
#�B
$B
#�B
#nB
$&B
$ZB
$ZB
%`B
$�B
%B
'8B
&�B
&�B
'B
'mB
(XB
(�B
(�B
(
B
(�B
)yB
(�B
*0B
*B
)�B
*B
)�B
)�B
,�B
.�B
.�B
-)B
,qB
,�B
/�B
/�B
/�B
0;B
0�B
0!B
0;B
0UB
/�B
.�B
/iB
/�B
0�B
0;B
/�B
/�B
0;B
0oB
0�B
0�B
1B
2-B
3MB
2�B
2B
2B
1�B
2-B
2|B
3�B
4TB
4�B
5%B
5tB
5�B
5ZB
5?B
5�B
5�B
5�B
6FB
6`B
6FB
6zB
6�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8B
7�B
7�B
7�B
7�B
8lB
8�B
9$B
9$B
9�B
9�B
:B
:xB
:�B
:�B
;dB
;�B
<B
="B
=<B
>]B
=�B
>(B
>]B
?HB
?�B
?�B
@B
@�B
@�B
@�B
?�B
>�B
?.B
@ B
>�B
>�B
>�B
?.B
?}B
@ B
@B
@B
@iB
@�B
@�B
AB
@�B
AUB
A�B
BuB
B�B
C�B
C�B
D3B
C�B
C�B
DB
D3B
DgB
D�B
D�B
EB
EB
E9B
EmB
E�B
F�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
IRB
I�B
I�B
I�B
J	B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
LJB
L~B
L~B
L�B
M6B
M�B
M�B
MPB
L�B
LJB
LB
L0B
L�B
M�B
MB
MB
L�B
L�B
L�B
MPB
NB
NpB
NpB
N<B
NVB
NB
M�B
MjB
MjB
NB
O\B
O�B
O�B
PHB
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
QhB
Q�B
RTB
SB
T�B
UB
UMB
U2B
UB
UMB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
WsB
W�B
W?B
WsB
WYB
WYB
WsB
W�B
XB
XB
X�B
YKB
Y�B
Y�B
ZB
Z7B
ZQB
ZkB
Z�B
Z�B
[#B
[�B
\]B
\xB
\�B
\�B
\�B
\�B
]/B
]dB
]~B
^B
^B
^jB
^5B
]�B
]�B
^B
^5B
^�B
^�B
^�B
^�B
_;B
_;B
_�B
_�B
_pB
_pB
_�B
_�B
_�B
`BB
`\B
`vB
`vB
`�B
`�B
aHB
a�B
a�B
bB
bB
b4B
bB
b4B
b4B
b�B
b�B
cnB
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
e,B
e,B
e`B
e�B
e�B
e�B
fB
fB
f2B
fLB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
h
B
g�B
h
B
h>B
h$B
hXB
h�B
iB
iB
iB
i*B
iyB
i�B
i�B
jeB
jB
j�B
j�B
kB
kB
kB
k6B
kkB
k�B
l"B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m]B
m]B
m�B
nB
nIB
n�B
n�B
n�B
n�B
o B
oB
o5B
oB
oOB
oOB
o�B
oiB
o�B
p!B
p�B
poB
p�B
p�B
p�B
qAB
qAB
q�B
q�B
q�B
q�B
rB
r-B
raB
r|B
r|B
r|B
r�B
r�B
r�B
r�B
s3B
s3B
s�B
s�B
tB
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
uB
uB
u?B
u�B
u�B
vB
vB
vB
v+B
u�B
u?B
utB
vzB
vB
vB
vB
v�B
wfB
w�B
xB
xlB
x�B
x�B
y	B
y$B
y>B
yXB
yrB
zDB
z^B
zxB
zxB
z�B
z�B
z�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<S\&<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005090048482020050900484820200509004848202207271135442022072711354420220727113544202207271538022022072715380220220727153802  JA  ARFMdecpA30a                                                                20200428064834  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200428064835  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200428064836  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200428064837  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200428064837  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200428064837  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200428064837                      G�O�G�O�G�O�                JA  ARUP                                                                        20200428065610                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200429000000  CF  PSAL_ADJUSTED_QC@�@���G�O�                JM  ARCAJMQC2.0                                                                 20200508154848  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200508154848  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023544  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063802  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                