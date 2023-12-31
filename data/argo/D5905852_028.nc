CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-09T09:38:51Z creation;2019-11-09T09:38:53Z conversion to V3.1;2022-08-02T05:11:51Z update;     
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
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191109093851  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_028                    2C  D   APEX                            8420                            2.11.2                          846 @���O� 1   @��V��@.b��}�co�E��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @���A   A   A@  A`  A�  A�  A�  A���A���A���A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Br  BvffB��B���B�ffB���B���B�33B�  B���B�  B�  B�  B�  B�  B�33B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C33C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV33CX�CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=q@|��@��@�{A33A?\)A_\)A33A��A��
A�=qA�=qAУ�A��HA��A��B��B�
B�HB�B'��B/�HB7�
B?�BG��BO�HBW�
B_�HBg��Bq��Bvz�B\)B��{B�L�B���B���B�{B��HB���B��
B��HB��B��HB��B�{B��RB�B�BýqB��
B��fB��HB��HB��B��fB��)B��HB���B�B�ffB�.B��{B��)B��HC�C�C  C�qC	��C�qC��C��C��C�3C�3C�C�RCC!HC�C!�3C#��C%�3C'��C)�C+�C-��C/�3C1�3C3�RC5��C7�C9�C;�3C=��C?��CA��CC�RCE�RCH�CI�qCK�RCM��CO��CQ��CS�RCV&fCX
=CY�C[��C]�C_�Ca�3Cc�3Ce�RCg�RCj  Ck�qCm�RCo�3Cq�3Cs��Cu�3Cw�Cy��C{�qC}�qC�qC��)C��)C��)C��RC���C���C���C�HC�  C��)C���C���C���C���C��qC�  C���C��qC��qC���C�  C��)C���C���C���C�  C�  C��)C��)C��)C���C��C��qC���C���C�HC��qC���C��RC��RC���C��)C���C���C���C��)C�HC��)C���C��)C���C���C��qC��)C��)C��)C��)C��)C��qC���C��)C���C��
C��)C��qC���C���C���C��)C�  C�  C���C���C���C��
C���C���C���C��)C��)C��qC���C��qC��)C�  C�  C��)C��RC���C��qC��qC���C�  C���C���C��qC���C��RC���C���C���C���C���C���C�C��)C��)C��qC���C���C���C��RC���C���C��)C���C��)C���C���C��)C���C���C��RC���C��)C���C���C��)D ~D ��D|�D��D� D  D}qD��D~D�\D~D�D}qD��D~�D �D�HD��D	~D	�\D
}qD
��D� D�D~�D��D~�D�\D}qD��D|)D��D|)D�\D�HD  D|�D�=D|)D��D� D�\D~�D�\D~D��D~D�)Dz�D�D~�D��Dz�D��D|)D�qD� D�\D~�D��D~D�qD}qD�\D ~�D �D!~�D!��D"~�D#  D#\D#�\D$\D$�D%|)D%��D&� D'  D'~D'�qD(\D)  D)\D)��D*~D+  D+}qD+��D,~�D,��D-~�D-��D.~D.�D/}qD/�)D0}qD0�D1}qD1�D2~D3  D3��D3�\D4|�D4�D5~D5�D6\D6�\D7~�D7�qD8|�D8�qD9|)D9�)D:~D:�\D;� D;��D<~D<��D=\D=�D>{�D>�)D?|�D?�D@\D@��DA~DA�DB{�DB��DC}qDC�qDD|�DD�DE|)DE��DF|�DF�DG\DG�DH~DH�DI{�DI��DJ}qDJ�qDK~DK��DL~DL��DM\DN �DN� DN��DO}qDO��DP|�DP��DQ\DQ��DR}qDR��DS~�DT  DT}qDT�qDU�HDV3DV~�DV��DW~DW�DX~DX��DY~�DY��DZ~DZ��D[~�D[�D\~D\�D]~D]�qD^|)D^�D_~D_��D`}qD`��Da\Da�Db~�Db��Dc}qDc��Dd}qDd�\De��De�Df|�Df��Dg}qDg��Dh~Dh��Di��Dj  Dj~�Dj��Dk~�Dk�qDl|)Dl�Dm}qDm��Dn|�Dn�Do��Do��Dp|�Dp�Dq~�Dq��Dr}qDr�Ds|�Ds��Dt|�Dt��Du\Du�qDv~�Dw �Dw��Dx �Dx~Dx�qDy��Dz  Dz~�D{ �D{�HD|  D|\D}  D}\D}�qD~|�D~��D~D�\D�>fD�~�D���D��D�>�D�~�D��
D�  D�?\D�\D��
D��D�?
D�~�D���D��\D�?\D�~fD���D���D�?
D�
D��
D��\D�?
D�
D��
D��D�?
D�� D���D��fD�>D�~D��fD��
D�?
D�}�D���D��
D�>�D�
D��
D� RD�@RD�
D��D���D�?�D�� D���D��
D�@ D��D���D���D�?
D�~D���D��
D�@ D��D���D�  D�?�D�
D���D��fD�>fD�~fD��fD��
D�>D�}�D��\D� RD�?�D�
D�� D��\D�?
D�
D�� D�  D�>fD�\D��\D���D�>�D�~�D���D��\D�?�D�
D��D���D�?\D�\D���D��
D�?�D�
D��
D��fD�>fD�~�D��fD���D�?\D�\D���D��\D�?�D���D���D� RD�>�D�~D��\D�  D�?
D�~fD���D��\D�@ D��D��fD��
D�@ D�
D��fD��D�>fD�
D���D���D�>fD�
D���D���D�>�D�
D���D��
D�>�D���D��\D��fD�?\D�~�D��fD��\D�?
D�
D���D���D�=�D�
D��\D� RD�>�D�}�D���D��\D�>fD�~D��
D�  D�@ D�~�D��qD��D�?
D�\D���D��
D�>�D�\D���D��
D�>�D�
D��
D���D�>�D�~D���D���D�>�D�\D���D���D�?�D�\D���D��
D�>fD�}qD��fD��
D�>�D�~�D���D��
D�>fD�~fD���D��fD�>fD�~�D���D��fD�?
D�\D���D���D�>�D�
D��\D��
D�?\D�� D���D���D�>�D�~fD��fD���D�?�D��D��fD���D�?
D��D��
D��fD�?�D�� D��
D��D�?
D�� D���D��\D�?\D�\D���D���D�>�D�\D�� D���D�=�D�}�D��
D� RD�@ D�� D��
D��D�>�D�~fD���D��
D�>�D�~�D¾�D��
D�>D�~�Dÿ\D���D�>�D�
DĿ
D��\D�?
D��D��HD� �D�?\D�~�DƾD��fD�>�D�
DǾfD��\D�@ D�~�DȾ�D��fD�>fD�
Dɿ�D���D�?
D�\D�� D��
D�>fD��D�� D���D�>fD�}�D̾�D��
D�>�D�
D;fD���D�?\D�\DξfD���D�>�D�}�DϾD��D�>fD�~�DоfD���D�?
D�
DѾD��fD�?\D�\Dҿ\D���D�?\D��Dӿ\D��
D�?
D�~fDԿ
D��fD�>fD�
Dվ�D��fD�>D�~D־fD��
D�>D�}D׾fD��D�>fD�\Dؿ\D��fD�?
Dـ Dٿ\D�  D�@�DڀRDڿ\D��\D�?�Dۀ D���D��\D�>fD�\Dܿ
D��fD�?\D�\Dݽ�D���D�>fD�~�D޿�D���D�>�D�
D߾fD��\D�@�D��RD�� D���D�?�D��D�� D� RD�?
D�~D�
D��fD�>D��D�\D���D�@ D�RD�\D��\D�@ D� D忮D���D�>fD�~D澸D��
D�?\D�
D�
D���D�?\D�
D辸D���D�?\D�~�D�
D���D�?�D�
D�fD��
D�>�D�
D�
D���D�?
D�
D쿮D��\D�?
D� D��\D��
D�>�D�
DD��
D�>�D�
D�\D� RD�?\D�~�D�
D���D�?
D�~�D�D���D�>�D�\D�D���D�=�D�~fD�
D��fD�>�D�~�D��fD��\D�@RD�� D��\D��\D�?�D�\D��
D��
D�=�D�~�D���D��
D�?
D�~�D��\D�  D�?
D�~D��\D���D�>fD�~�D���D��
D�@ D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{JA�k�A�y�A�%�A���A��TA�یA�՛A���A�͟A��pA���A�ɺA��A�n�A�W�AߤtA�d&A�WsA�A�A�,A��A��A��sA��A�چAީ�Aނ�A�X�A�lWA�M�A�c�A��TA��AΘ�A�DgA�A�c A��7A���A�.�A���A�:^A�VA�f�A���A��vA�k�A�
=A�2�A���A�8�A��A��dA�zxA��0A�~A���A��A�_A��^A��^A���A��A���A��HA�I�A�"A�1�A���A�0�A�N�A�ɆA�a�A��A|Au?�Ao��Aj�fAgF�Aa�A\K^AX�AS��APFtAM�$AK
=AIqvAFK^ADOAB�AA��AA�A?=�A=�A<V�A;�4A:	lA8�A8�HA7��A6��A4�A1�A/8�A,��A+xA*��A'�A%JA#�A#Z�A"�A"A A"�'A"r�A!��A A��AM�A~A��A��Ay�A�A�0Av�A�$A�_A7�A��A�HA��AL0A1Ar�A��A�zAA$A��An�AZ�ACA�`Ay�AA�A�MA�A��A!�AqA�AV�A�Ac�A*0AA�A�$AZA��A4�A�0A�qA�@Ab�A��AL�Ae�A
��A
K^A	�HA	��A	C�A	�ARTAR�A�VAMjA��AC-A�RAP�A��A�fA��AOA�IAA��A�A �$A ��A �FA �@���@��@��$@��b@�S&@���@�L0@���@�\�@�
=@�m�@��"@��@�#:@�8@�>B@�v`@�1'@�rG@��M@���@��@��;@��@@���@�B�@�q@���@�x@�0@�;d@�u�@�@��@��m@�/�@��@���@�8�@��p@��j@��@��@��@��r@�W?@ބ�@�S�@݈f@�u�@�.�@��@�ƨ@ۜ�@�8�@ړu@��@ن�@��v@�N�@��@�_p@�PH@��@�Vm@�)_@���@�U2@ӷ@�&�@�u�@ю�@�=�@�@��@�@��?@Е@�?�@���@���@Ϝ�@�H�@�:�@�@Β�@�x�@���@�c�@ˣn@��@ʌ@��@ɧ�@��@�w�@�x@�[W@��@��	@�B[@�ϫ@ũ*@�!�@�:*@�@��d@Ê�@Á�@é*@�^�@�@�W�@�7@��@@��@���@���@�YK@�'R@���@��@�[�@��@��Q@��@�y�@�j�@�RT@��@�C�@��@�IR@���@�$�@�@��@���@�F�@��@��@���@�A @���@���@���@�� @�[�@��+@���@��f@��@��u@��D@��@�˒@�rG@�@�m�@��r@��w@��S@�b�@�/@��@��@�w�@�"h@��a@�_p@�#�@��5@��A@�@���@���@��C@���@�c�@�Mj@�,�@���@�m�@�!�@���@��"@�IR@�@��]@���@�oi@�9X@�b@��0@�IR@��5@�Q�@���@���@��f@�b�@� \@�)�@��Q@���@�\)@��@�Ɇ@���@�h�@�1�@�_@��D@��W@��[@��@��6@��.@��
@��^@��k@���@��$@���@��@�}�@��@�tT@�"h@�X@�ߤ@���@��_@�R�@��z@�~�@�H�@��@��9@�6�@���@��@���@��M@�>�@���@�D�@���@���@��4@�\�@��@���@�L0@�4@��)@�˒@�@���@�j�@���@���@��4@��F@���@�z@�h
@���@��'@�|�@�L�@��?@��I@�{�@�2�@��&@�ԕ@���@��F@��-@�u�@�Mj@�7L@�"�@��	@���@�?�@���@���@��-@���@�F�@�+@�Ɇ@�tT@�#:@�ԕ@�o�@�Q�@�!-@��@���@�L0@�M@��A@���@��3@�rG@�V@��e@��@�oi@�R�@�4@��N@���@���@�p�@��@��B@���@�D�@���@��@��@���@��4@�n�@�<�@�4@�x�@�	l@���@���@�l�@�,=@��@��d@��:@�T�@� \@�@@���@���@�Q�@�:*@��@��@���@���@�6z@��"@�҉@�g8@�+k@�"h@�b@�u@��H@�E9@��H@���@�Z�@�[@�@~��@~xl@~YK@~:*@}�.@}�@}��@}/@|��@{�@{qv@{'�@z��@zkQ@zO@y�@xѷ@x��@x(�@w�a@wqv@wJ#@w33@w(@v�m@vOv@u�@u@u�M@uB�@u�@t*�@sv`@s�@r+k@q�S@q8�@q+�@q0�@q%F@q@p�`@p��@o��@o�@nn�@nE�@n&�@m�@m��@m��@mzx@m&�@l�/@l��@l�@k(@j��@j_�@i��@i/@h�?@hN�@g�K@g>�@f��@f� @fJ�@f($@e�@e��@e8�@d4n@cb�@b�8@b�]@b� @b��@bi�@b�@bu@a��@aw2@aS&@`��@`1@_�6@_��@_v`@_4�@^�8@^��@^Ta@^($@^ �@]�t@]T�@\�K@\6@[ƨ@[\)@[=@Z��@Z0U@Z	@Y�t@Y:�@XĜ@X]d@X/�@X�@W��@W��@Wg�@W�@V��@Vl�@V6�@U�=@UL�@U+�@U;@T�)@T/�@S,�@R͟@R��@R3�@Q�Z@Q�@Q��@Q�7@QF@P�@P��@P�@O��@O�@Nq�@M��@M[W@L�5@Lj@K�;@K�@Kqv@K4�@J�@I��@I�-@I��@Ij@H�P@Hw�@G�6@Gs@F�]@Fi�@F.�@F&�@F{@E�.@E��@E�@D�|@D��@DS�@D�@C�W@C;d@B҉@Bxl@B
�@A�@AS&@@�@@q@@Xy@?�@>��@>i�@=��@=�N@=?}@<�5@<�@<��@<m�@<>B@<-�@<�@;�@;��@;\)@:��@:M�@9��@9`B@9#�@8�)@8oi@7�]@7��@7�@7��@7��@7y�@7=@6��@6?@6	@5��@5��@57L@4�@4z�@3�r@3�@@3qv@3l�@3g�@3Mj@31�@2�B@2��@2!�@1�t@1L�@1�@0��@0c�@/�+@/�
@/�[@/�k@/y�@/P�@.��@.+k@-�D@-�@-q@,֡@,Ĝ@,�z@,m�@,>B@+�r@+��@+W?@+)_@+�@+v`@+S�@+6z@*��@*C�@*�@)��@)��@)�=@)\�@(�@(�p@(��@(y>@(c�@(�@'�@'�@'X�@')_@&�@&�B@&�x@&c @&O@%�>@%��@%|@%k�@%A @%@@$ی@$��@$c�@$:�@$�@$�@#��@#~�@#K�@#C�@"�]@"�\@"@�@"!�@"e@"�@!@!�h@!f�@!X@!N<@!J�@!2a@!	l@ �p@ �@ l"@ Q�@ 2�@ �@�}@l�@S�@;d@'�@o@ߤ@�6@�@��@hs@<6@�@�@�@�@%@�@�|@��@?�@'R@�@��@�{@&@��@�b@W�@0U@�Z@�@��@@�@��@[W@�@��@l"@e�@Xy@K^@b@�@��@�{@@O@/�@&@ߤ@�@d�@��@��@c�@G�@&�@��@��@q@�@��@{J@9�@�@ i@��@ȴ@�h@Q@6�@-@{@�z@c@O�@L�@IR@IR@A @@�K@�U@��@�o@/�@�&@�@�@�}@j�@]�@x@J#@�]@҉@��@xl@^5@3�@!�@�@J@�@��@��@�@�X@zx@o @k�@Vm@8�@�@�@��@Xy@N�@7�@�}@t�@)_@@S@
ߤ@
�s@
҉@
҉@
��@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{JA�k�A�y�A�%�A���A��TA�یA�՛A���A�͟A��pA���A�ɺA��A�n�A�W�AߤtA�d&A�WsA�A�A�,A��A��A��sA��A�چAީ�Aނ�A�X�A�lWA�M�A�c�A��TA��AΘ�A�DgA�A�c A��7A���A�.�A���A�:^A�VA�f�A���A��vA�k�A�
=A�2�A���A�8�A��A��dA�zxA��0A�~A���A��A�_A��^A��^A���A��A���A��HA�I�A�"A�1�A���A�0�A�N�A�ɆA�a�A��A|Au?�Ao��Aj�fAgF�Aa�A\K^AX�AS��APFtAM�$AK
=AIqvAFK^ADOAB�AA��AA�A?=�A=�A<V�A;�4A:	lA8�A8�HA7��A6��A4�A1�A/8�A,��A+xA*��A'�A%JA#�A#Z�A"�A"A A"�'A"r�A!��A A��AM�A~A��A��Ay�A�A�0Av�A�$A�_A7�A��A�HA��AL0A1Ar�A��A�zAA$A��An�AZ�ACA�`Ay�AA�A�MA�A��A!�AqA�AV�A�Ac�A*0AA�A�$AZA��A4�A�0A�qA�@Ab�A��AL�Ae�A
��A
K^A	�HA	��A	C�A	�ARTAR�A�VAMjA��AC-A�RAP�A��A�fA��AOA�IAA��A�A �$A ��A �FA �@���@��@��$@��b@�S&@���@�L0@���@�\�@�
=@�m�@��"@��@�#:@�8@�>B@�v`@�1'@�rG@��M@���@��@��;@��@@���@�B�@�q@���@�x@�0@�;d@�u�@�@��@��m@�/�@��@���@�8�@��p@��j@��@��@��@��r@�W?@ބ�@�S�@݈f@�u�@�.�@��@�ƨ@ۜ�@�8�@ړu@��@ن�@��v@�N�@��@�_p@�PH@��@�Vm@�)_@���@�U2@ӷ@�&�@�u�@ю�@�=�@�@��@�@��?@Е@�?�@���@���@Ϝ�@�H�@�:�@�@Β�@�x�@���@�c�@ˣn@��@ʌ@��@ɧ�@��@�w�@�x@�[W@��@��	@�B[@�ϫ@ũ*@�!�@�:*@�@��d@Ê�@Á�@é*@�^�@�@�W�@�7@��@@��@���@���@�YK@�'R@���@��@�[�@��@��Q@��@�y�@�j�@�RT@��@�C�@��@�IR@���@�$�@�@��@���@�F�@��@��@���@�A @���@���@���@�� @�[�@��+@���@��f@��@��u@��D@��@�˒@�rG@�@�m�@��r@��w@��S@�b�@�/@��@��@�w�@�"h@��a@�_p@�#�@��5@��A@�@���@���@��C@���@�c�@�Mj@�,�@���@�m�@�!�@���@��"@�IR@�@��]@���@�oi@�9X@�b@��0@�IR@��5@�Q�@���@���@��f@�b�@� \@�)�@��Q@���@�\)@��@�Ɇ@���@�h�@�1�@�_@��D@��W@��[@��@��6@��.@��
@��^@��k@���@��$@���@��@�}�@��@�tT@�"h@�X@�ߤ@���@��_@�R�@��z@�~�@�H�@��@��9@�6�@���@��@���@��M@�>�@���@�D�@���@���@��4@�\�@��@���@�L0@�4@��)@�˒@�@���@�j�@���@���@��4@��F@���@�z@�h
@���@��'@�|�@�L�@��?@��I@�{�@�2�@��&@�ԕ@���@��F@��-@�u�@�Mj@�7L@�"�@��	@���@�?�@���@���@��-@���@�F�@�+@�Ɇ@�tT@�#:@�ԕ@�o�@�Q�@�!-@��@���@�L0@�M@��A@���@��3@�rG@�V@��e@��@�oi@�R�@�4@��N@���@���@�p�@��@��B@���@�D�@���@��@��@���@��4@�n�@�<�@�4@�x�@�	l@���@���@�l�@�,=@��@��d@��:@�T�@� \@�@@���@���@�Q�@�:*@��@��@���@���@�6z@��"@�҉@�g8@�+k@�"h@�b@�u@��H@�E9@��H@���@�Z�@�[@�@~��@~xl@~YK@~:*@}�.@}�@}��@}/@|��@{�@{qv@{'�@z��@zkQ@zO@y�@xѷ@x��@x(�@w�a@wqv@wJ#@w33@w(@v�m@vOv@u�@u@u�M@uB�@u�@t*�@sv`@s�@r+k@q�S@q8�@q+�@q0�@q%F@q@p�`@p��@o��@o�@nn�@nE�@n&�@m�@m��@m��@mzx@m&�@l�/@l��@l�@k(@j��@j_�@i��@i/@h�?@hN�@g�K@g>�@f��@f� @fJ�@f($@e�@e��@e8�@d4n@cb�@b�8@b�]@b� @b��@bi�@b�@bu@a��@aw2@aS&@`��@`1@_�6@_��@_v`@_4�@^�8@^��@^Ta@^($@^ �@]�t@]T�@\�K@\6@[ƨ@[\)@[=@Z��@Z0U@Z	@Y�t@Y:�@XĜ@X]d@X/�@X�@W��@W��@Wg�@W�@V��@Vl�@V6�@U�=@UL�@U+�@U;@T�)@T/�@S,�@R͟@R��@R3�@Q�Z@Q�@Q��@Q�7@QF@P�@P��@P�@O��@O�@Nq�@M��@M[W@L�5@Lj@K�;@K�@Kqv@K4�@J�@I��@I�-@I��@Ij@H�P@Hw�@G�6@Gs@F�]@Fi�@F.�@F&�@F{@E�.@E��@E�@D�|@D��@DS�@D�@C�W@C;d@B҉@Bxl@B
�@A�@AS&@@�@@q@@Xy@?�@>��@>i�@=��@=�N@=?}@<�5@<�@<��@<m�@<>B@<-�@<�@;�@;��@;\)@:��@:M�@9��@9`B@9#�@8�)@8oi@7�]@7��@7�@7��@7��@7y�@7=@6��@6?@6	@5��@5��@57L@4�@4z�@3�r@3�@@3qv@3l�@3g�@3Mj@31�@2�B@2��@2!�@1�t@1L�@1�@0��@0c�@/�+@/�
@/�[@/�k@/y�@/P�@.��@.+k@-�D@-�@-q@,֡@,Ĝ@,�z@,m�@,>B@+�r@+��@+W?@+)_@+�@+v`@+S�@+6z@*��@*C�@*�@)��@)��@)�=@)\�@(�@(�p@(��@(y>@(c�@(�@'�@'�@'X�@')_@&�@&�B@&�x@&c @&O@%�>@%��@%|@%k�@%A @%@@$ی@$��@$c�@$:�@$�@$�@#��@#~�@#K�@#C�@"�]@"�\@"@�@"!�@"e@"�@!@!�h@!f�@!X@!N<@!J�@!2a@!	l@ �p@ �@ l"@ Q�@ 2�@ �@�}@l�@S�@;d@'�@o@ߤ@�6@�@��@hs@<6@�@�@�@�@%@�@�|@��@?�@'R@�@��@�{@&@��@�b@W�@0U@�Z@�@��@@�@��@[W@�@��@l"@e�@Xy@K^@b@�@��@�{@@O@/�@&@ߤ@�@d�@��@��@c�@G�@&�@��@��@q@�@��@{J@9�@�@ i@��@ȴ@�h@Q@6�@-@{@�z@c@O�@L�@IR@IR@A @@�K@�U@��@�o@/�@�&@�@�@�}@j�@]�@x@J#@�]@҉@��@xl@^5@3�@!�@�@J@�@��@��@�@�X@zx@o @k�@Vm@8�@�@�@��@Xy@N�@7�@�}@t�@)_@@S@
ߤ@
�s@
҉@
҉@
��@
�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�HB�hB��B��B�FB�{BԕB��B��B՛B�mB�B��B�B�RB	�PB	��B	՛B	��B	ܬB	ߊB	�B	�B	�B	��B
UB
�B
-B
�B	�JB	�B	�B
mB
49B
7�B
X+B
�B
�fB
�HBB
ٚB
�CB
�oB
��B
�B
յB
ԯB
��B
�;B6BU�BMjB;B/�BB�B
�|B
��B
�dB
��B
�hB
̳B
��B
�B
q�B
R�B
H�B
JXB
8�B
	lB
�B	�B	�6B	�:B	�^B	��B	��B	m�B	P.B	?�B	*B	@B��B�B�BңBǮB��B�fB��B�OB�B��B�DB�qB��B�cB��B�B�vB�|B�IB�B��B�mB��B�]B�;B��B��B͹B՛B�B�5B��B	�B	qB	�B	!�B	.�B	7�B	>�B	F�B	L0B	YKB	eFB	z^B	�=B	��B	�@B	��B	��B	��B	�ZB	�B	��B	��B	�%B	�B	�UB	��B	�nB	�tB	��B	�B	��B	�rB	�^B	��B	��B	�B	��B	ňB	�B	� B	�VB	�B	�}B	� B	ĜB	��B	��B	�XB	��B	��B	��B	̈́B	ЗB	�vB	�"B	�=B	��B	ȴB	ȚB	�RB	�)B	̘B	��B	οB	бB	�(B	�B	�<B	ΥB	�(B	�B	�BB	��B	��B	ѷB	��B	��B	өB	ҽB	�,B	��B	��B	ңB	өB	�[B	�B	�uB	�uB	�uB	��B	��B	ӏB	��B	�{B	�,B	ԯB	ԯB	�{B	��B	�MB	՛B	յB	��B	��B	յB	�2B	�SB	ևB	ևB	�9B	�SB	ּB	��B	�$B	�?B	רB	�sB	רB	׍B	�EB	�KB	�KB	��B	خB	�yB	خB	�B	��B	��B	�eB	��B	�1B	��B	��B	�B	�B	��B	��B	�QB	چB	ڠB	��B	ڠB	�QB	�kB	ںB	ڠB	��B	��B	�	B	�WB	�	B	ںB	ܬB	ܬB	�IB	��B	��B	ܬB	�/B	�dB	��B	�B	��B	�B	�5B	�B	ݲB	��B	��B	�jB	��B	ߤB	��B	�vB	��B	�B	�-B	�HB	�B	�4B	�B	�B	�:B	�:B	��B	�TB	��B	��B	�B	��B	�2B	�
B	��B	�B	�eB	�B	�B	��B	�=B	�)B	��B	�B	�qB	��B	�cB	�OB	�iB	�B	��B	�-B	��B	�nB	�?B	�nB	��B	��B	�%B	�FB	�fB	�	B	��B	��B	�B	�TB	�tB	��B	�`B	�B	�	B	�	B	�xB	�dB	�JB	��B	�<B	�wB	��B	��B
  B
�B
'B
�B
�B
B
�B
�B
gB
gB
�B
�B
B
�B
�B
B
�B
mB
B
�B
SB
B
B
�B
�B
YB
tB
�B
�B
�B
B
�B
+B
�B
fB
�B
fB
	B
	�B
	�B
	�B
�B

XB

�B
B
xB
�B
�B
�B
dB
�B
B
B
jB
�B
jB
jB
6B
PB
B
�B
�B
B
�B
\B
�B
�B
�B
�B
�B
HB
NB
�B
:B
�B
4B
�B
B
�B
 B
�B
TB
:B
oB
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
�B
eB
�B
B
�B
=B
WB
�B
�B
=B
�B
CB
]B
xB
xB
xB
~B
�B
�B
!B
jB
5B
5B
B
;B
�B
 B
 B
�B
 \B
 \B
 \B
 B
�B
pB
�B
 vB
 �B
 �B
!-B
!�B
!�B
"hB
"�B
#:B
#�B
$�B
$�B
$�B
%zB
%�B
&2B
&fB
&�B
&�B
&�B
&�B
'B
'mB
'�B
'mB
'�B
'�B
(sB
(sB
(�B
(�B
)_B
)�B
)DB
)�B
*�B
+�B
+�B
,"B
,�B
-wB
-�B
.B
.�B
/5B
/iB
/�B
/�B
0UB
0oB
0�B
1B
1�B
1�B
1�B
2B
3�B
3�B
4B
4B
49B
4nB
4nB
4�B
4�B
4�B
5%B
5ZB
5?B
5tB
5ZB
5�B
6B
5�B
5ZB
5�B
5tB
5B
5�B
6�B
7LB
7fB
7�B
7�B
7�B
88B
8�B
9>B
9XB
9rB
9�B
9�B
9�B
:B
:^B
:^B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;0B
;0B
;0B
;0B
;JB
;B
;�B
;�B
;�B
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>B
>BB
>BB
>]B
>wB
>�B
>wB
>�B
?cB
?cB
?�B
?�B
?�B
@ B
@ B
@4B
@OB
@�B
@�B
@�B
@�B
@�B
@�B
AB
A;B
A�B
BB
BB
BAB
BAB
B�B
CB
CB
C-B
C-B
CB
C�B
DB
DMB
D�B
D�B
EB
EB
EmB
E�B
E�B
E�B
F%B
FYB
FtB
GB
G+B
G_B
GB
GzB
G�B
G�B
G�B
H1B
HfB
H�B
H�B
H�B
H�B
IB
IB
IB
I�B
I�B
I�B
J#B
J=B
J=B
JXB
J=B
J�B
K�B
K�B
K�B
K�B
K�B
LB
LJB
L0B
LdB
L�B
L�B
MB
MPB
MPB
M�B
M�B
M�B
M�B
N"B
N�B
N�B
N�B
N�B
O�B
O�B
PB
PB
PB
PbB
P}B
Q4B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
SB
S[B
SuB
SuB
TaB
T�B
T�B
T�B
UMB
U�B
VB
VB
VB
VmB
W?B
W�B
XB
W�B
XyB
X�B
X�B
X�B
Y1B
YKB
YKB
YeB
YB
YB
Y�B
Z7B
ZQB
Z�B
[	B
[=B
[�B
\xB
\�B
\�B
\�B
]/B
]dB
]�B
]�B
^5B
^OB
^�B
^�B
^�B
_!B
_;B
_pB
_�B
_�B
`B
`B
`B
`'B
`'B
`vB
`�B
aB
aHB
abB
abB
a�B
b4B
bhB
bNB
bhB
bhB
bhB
b�B
b�B
c�B
dZB
d�B
eFB
e`B
eFB
eFB
eFB
e�B
e�B
e�B
f�B
g�B
h�B
i_B
iyB
i�B
jB
j�B
kB
k6B
k6B
k6B
kQB
k�B
k�B
k�B
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
l�B
mB
mB
m)B
mCB
mwB
m�B
m�B
m�B
n/B
nIB
n/B
nIB
ncB
n�B
n�B
n�B
o5B
o5B
oOB
oiB
oOB
o�B
o�B
pUB
poB
p�B
p�B
p�B
qB
q'B
qB
q'B
qAB
q'B
q'B
p�B
p�B
p�B
q[B
raB
r�B
sMB
s�B
s�B
s�B
s�B
s�B
tnB
t�B
t�B
uB
u?B
u?B
uZB
uZB
uZB
uZB
u?B
u�B
u�B
u�B
u�B
u�B
vB
vB
v+B
v`B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
w2B
wLB
wfB
w�B
w�B
xB
x8B
x�B
x�B
x�B
y	B
y	B
y$B
yrB
y�B
y�B
zB
y�B
y�B
zB
zB
zDB
z�B
z�B
{0B
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}VB
~B
}�B
}�B
}�B
~B
}�B
}�B
~(B
~�B
B
.B
.B
.B
.B
cB
HB
}B
�B
}B
�B
�B
� B
� B
�B
�4B
�4B
�B
�B
�B
�iB
��B
��B
��B
�'B
�AB
�'B
�[B
�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BңB��B�uB�FB�aBԕBԕB��B��B՛BևB�+B�B�B��B	��B	ؓB	��B	�B	��B	��B	��B	�3B	��B	��B
�B
uB
3B
B
 �B	�xB	�B
)B
6�B
FB
a�B
��B
�"BtB	B
�B
�vB
�ZB
�6B
�YB
֡B
�YB
�=B
�3B9rBX�BQ�B!�B2-B�BB
��B
ބB
��B
�vB
��B
��B
��B
��B
u?B
VB
J�B
O�B
?B
�B
�B	�BB	�B	��B	�B	��B	�9B	s�B	U�B	F�B	0�B	�B	B�AB�|B��B�XBĜB��B��B��B��B�_B��B��B��B�!B��B��B�B��B��B��B��B�KB�_B�5B�TB�	B�3BΥB֡B��B�5B	 iB	�B	�B	�B	"�B	/B	88B	>�B	F�B	L�B	Y�B	e�B	{�B	�^B	�TB	�aB	��B	��B	�bB	��B	��B	��B	��B	�B	��B	��B	�|B	��B	��B	�fB	��B	�$B	��B	�xB	�VB	�B	��B	ŢB	�%B	��B	��B	��B	�B	��B	��B	�B	ɆB	ˬB	��B	�lB	�B	�EB	��B	�NB	�}B	�\B	�)B	ȚB	�7B	�B	��B	��B	�PB	�0B	ϑB	�NB	�B	ϑB	��B	�BB	��B	ϫB	�.B	��B	ѷB	҉B	ңB	ՁB	�B	��B	ԕB	՛B	ԯB	�&B	�FB	�B	��B	��B	��B	��B	�,B	�B	�B	�{B	��B	��B	ՁB	ՁB	�2B	��B	��B	�B	�B	�9B	�B	�B	��B	��B	��B	ּB	ևB	��B	�
B	�sB	רB	��B	��B	��B	�EB	�_B	�1B	ٴB	ٴB	ٚB	�eB	�KB	�B	ٴB	�B	�B	ٴB	ٚB	��B	�B	�B	�B	�QB	�7B	�kB	ںB	��B	�#B	�WB	��B	��B	�#B	�#B	��B	�	B	�=B	�qB	��B	ۦB	�=B	�/B	��B	�dB	��B	��B	��B	�~B	ݘB	�B	�B	�B	�OB	�OB	�5B	�5B	ބB	�;B	��B	�pB	�B	�vB	��B	�HB	�B	�B	�B	�B	�B	�B	�:B	�B	�nB	�TB	��B	�B	�B	�@B	��B	�2B	�XB	�yB	��B	�B	�B	��B	�=B	�qB	�]B	�)B	�B	��B	�]B	�B	�B	�B	�B	�'B	�aB	�MB	��B	��B	��B	�B	��B	�ZB	��B	��B	�XB	�$B	�?B	�TB	��B	��B	�+B	�zB	�8B	�>B	�XB	��B	��B	��B	��B	�qB	��B	��B	��B
 OB
B
uB
�B
�B
GB
�B
�B
�B
�B
B
9B
SB
B
B
mB
�B
�B
SB
B
mB
%B
?B
%B
?B
�B
�B
B
B
B
EB
+B
_B
�B
�B
�B
�B
	�B

#B

=B

	B
	7B

rB
B
^B
0B
�B
�B
0B
�B
�B
jB
B
�B
�B
�B
�B
�B
�B
VB
VB
�B
(B
(B
\B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
NB
B
�B
�B
TB
�B
�B
�B
�B
�B
�B
�B
B
EB
_B
$B
�B
?B
�B
+B
1B
�B
B
7B
�B
WB
qB
]B
�B
qB
�B
]B
�B
�B
�B
�B
�B
�B
�B
pB
�B
jB
jB
;B
VB
�B
 BB
 BB
 'B
 vB
 vB
 vB
 BB
�B
�B
 'B
 �B
 �B
!HB
!|B
!�B
"B
"�B
#B
#�B
$&B
$�B
$�B
%,B
%�B
&B
&�B
&�B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
'�B
($B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*eB
+6B
+�B
+�B
,=B
,�B
-�B
-�B
.}B
/OB
/iB
/�B
/�B
0B
0oB
0�B
1B
1AB
1�B
1�B
1�B
2aB
3�B
4B
49B
49B
4TB
4�B
4�B
5%B
5B
5B
5?B
5tB
5ZB
5�B
5�B
5�B
6`B
5�B
5�B
6+B
5�B
5?B
5�B
6�B
7�B
7�B
7�B
7�B
8B
8lB
8�B
9�B
9rB
9�B
9�B
9�B
9�B
:DB
:�B
:�B
:�B
:�B
;B
;B
;B
:�B
;0B
;JB
;dB
;dB
;JB
;B
;dB
<B
<B
;�B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=B
=qB
=�B
=�B
>B
=�B
>(B
>wB
>]B
>wB
>�B
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@B
@4B
@4B
@iB
@�B
@�B
@�B
@�B
@�B
AB
A B
AoB
A�B
A�B
B'B
B'B
BAB
B[B
B�B
C-B
C-B
CGB
CGB
CaB
C�B
D3B
D�B
D�B
EB
E9B
E9B
E�B
E�B
E�B
E�B
FYB
F�B
F�B
GEB
G_B
G�B
G_B
G�B
G�B
G�B
HB
HfB
H�B
H�B
IB
IB
H�B
IB
I7B
IlB
I�B
I�B
I�B
J=B
JrB
JrB
JrB
J�B
KB
K�B
K�B
K�B
K�B
LB
L0B
LdB
LdB
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
NB
NpB
N�B
N�B
OB
O(B
O�B
PB
P.B
P.B
PHB
P�B
P�B
Q�B
QhB
Q�B
Q�B
RB
R B
RB
RB
R�B
R�B
SB
S&B
SuB
S�B
S�B
T�B
T�B
T�B
UB
U�B
U�B
V9B
VB
V9B
V�B
WsB
W�B
X+B
XB
X�B
X�B
X�B
X�B
YKB
YKB
YeB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
[#B
[#B
[WB
\)B
\�B
\�B
\�B
]B
]dB
]~B
]�B
^B
^jB
^jB
^�B
^�B
_B
_;B
_pB
_�B
_�B
`B
`B
`'B
`'B
`BB
`\B
`�B
`�B
aHB
a|B
a|B
a�B
a�B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c:B
c�B
dtB
eB
e`B
e`B
e`B
ezB
e`B
e�B
fB
e�B
f�B
gmB
h�B
i�B
i�B
jB
j�B
j�B
k6B
kQB
kQB
kQB
kkB
k�B
k�B
k�B
lB
k�B
l"B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
ncB
ncB
nIB
ncB
n}B
n�B
n�B
n�B
oOB
oOB
oiB
o�B
oiB
o�B
o�B
poB
p�B
p�B
p�B
p�B
q'B
q'B
q'B
qAB
q[B
qAB
qAB
p�B
p�B
p�B
qvB
r|B
r�B
shB
s�B
s�B
s�B
s�B
tB
t�B
uB
t�B
uB
u?B
u?B
uZB
utB
utB
utB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v+B
vFB
vzB
v�B
v�B
wB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
y>B
y>B
yXB
y�B
y�B
zB
z*B
zB
z*B
z*B
zDB
z^B
z�B
{B
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|B
|B
|B
|B
|B
{�B
{�B
{�B
{�B
|B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}qB
~(B
~B
}�B
~B
~(B
}�B
}�B
~(B
~�B
B
.B
.B
.B
cB
}B
cB
�B
�B
�B
�B
�B
�B
�B
�4B
�4B
�OB
�OB
�B
�4B
�iB
��B
��B
��B
�'B
�[B
�'B
�[B
��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<`u�<#�
<#�
<g�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.03(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911200046592019112000465920191120004659202207271133302022072711333020220727113330202207271536002022072715360020220727153600  JA  ARFMdecpA30a                                                                20191109093731  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191109093851  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191109093852  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191109093853  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191109093853  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191109093853  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191109093853                      G�O�G�O�G�O�                JA  ARUP                                                                        20191109095434                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191110000000  CF  PSAL_ADJUSTED_QC@(�@~�RG�O�                JM  ARCAJMQC2.0                                                                 20191119154659  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191119154659  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023330  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063600  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                