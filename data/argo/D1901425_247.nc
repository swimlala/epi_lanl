CDF   
   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_CALIB       N_LEVELS   8   	N_HISTORY                title         Argo float vertical profile    institution       AOML   source        
Argo float     history       p2016-10-03T17:35:15Z Decoding & Creation; 2017-04-14T16:07:52Z DMQC & Calibration; 2018-01-16T16:03:16Z Timing;    
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment       	free text      comment_on_resolution         �The profile TEMP and PSAL data resolution can be different than nominal. The data packing algorithm requires lower resolution be used to accommodate high vertical gradients. Values of 0.002, 0.004, & 0.008 are typical.     !comment_on_dmqc_responsible_party         BJohn Gilson (jgilson@ucsd.edu) Scripps Institution of Oceanography        @   	DATA_TYPE                  conventions       Argo reference table 1     	long_name         	Data type      
_FillValue                    ;�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    ;�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    ;�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    ;�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    ;�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    ;�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    ;�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  ;�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  <   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  <\   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        <�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    <�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    <�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     <�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    <�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    <�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     <�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     <�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     =    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =    JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   axis      T      
_FillValue        A.�~            =$   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =,   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            =0   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =@   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    =H   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    =L   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    =T   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    =X   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    =\   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    =`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        >`   PRES               
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  >d   PRES_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  ?D   PRES_ADJUSTED                  
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     axis      Z      
_FillValue        G�O�      �  ?|   PRES_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  @\   PRES_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�     
_FillValue        G�O�      �  @�   TEMP               
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   comment_on_resolution         �TEMP resolution may be more coarse than 0.004 degree C due to data packing of strong vertical gradients, or less coarse due to packing within weak vertical gradients      
_FillValue        G�O�      �  At   TEMP_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  BT   TEMP_ADJUSTED                  
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   comment_on_resolution         �TEMP_ADJUSTED resolution may be more coarse than 0.004 degree C due to data packing of strong vertical gradients, or less coarse due to packing within weak vertical gradients     
_FillValue        G�O�      �  B�   TEMP_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  Cl   TEMP_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  C�   PSAL               
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   comment_on_resolution         �PSAL resolution may be more coarse than 0.004 psu due to data packing of strong vertical gradients, or less coarse due to packing within weak vertical gradients   
_FillValue        G�O�      �  D�   PSAL_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  Ed   PSAL_ADJUSTED                  
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ;�o   comment_on_resolution         �PSAL_ADJUSTED resolution may be more coarse than 0.004 psu due to data packing of strong vertical gradients, or less coarse due to packing within weak vertical gradients      
_FillValue        G�O�      �  E�   PSAL_ADJUSTED_QC                  	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  8  F|   PSAL_ADJUSTED_ERROR                   	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�      �  F�   	PARAMETER            
   	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  G�   SCIENTIFIC_CALIB_EQUATION            
   	            	long_name         'Calibration equation for this parameter    
_FillValue                    G�   SCIENTIFIC_CALIB_COEFFICIENT         
   	            	long_name         *Calibration coefficients for this equation     
_FillValue                    J�   SCIENTIFIC_CALIB_COMMENT         
   	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    M�   SCIENTIFIC_CALIB_DATE            
   	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  P�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    P�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    P�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    P�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    P�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Q    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    Q@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    QP   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    QT   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        Qd   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        Qh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Ql   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    QpArgo profile    3.1 1.2 19500101000000  20161003173515  20180116160316  1901425 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   PRES            TEMP            PSAL               �A   AO  3571                            2C  D   SOLO                            2908                            V1.2; SBE601 16Mar07            851 @��}R�&W8   @�ς���C���R@D-O�;dZ1   ARGOS   A   A   A   Primary sampling: averaged [data averaged with equal weights into irregular pressure bins, sampled at 0.5 Hz from a SBE41CP]                                                                                                                                       @�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C1  CO  Cm  C�� C�� C�� C�� C�� CЀ C߀ C� C�� D@ D� D@ D � D/� D>� DM� D\� Dk� Dz� D�� D�@ D�� D�@ D�� D�@ D�� 11111111111111111111111111111111111111111111111111111111@�  A  Ap  A�  A�  B  B  B4  BL  Bd  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C  C  C  C  C1  CO  Cm  C�� C�� C�� C�� C�� CЀ C߀ C� C�� D@ D� D@ D � D/� D>� DM� D\� Dk� Dz� D�� D�@ D�� D�@ D�� D�@ D�� 11111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ah�9Ah��Ah�/Ag�wAcC�ASoAF=qAE��AEXAE�AD�jAD  AA�A?�PA=�hA9�A7K�A4��A1��A01'A0��A0bA0�A1��A3�FA5/A5hsA++A��A!��A$1'A&�\A��A/A�-AXA��A
VAdZ@�=q@�33@ܼj@���@��@���@���@��@�\)@��F@{"�@q&�@h1'@^��@Vȴ@O�;@G+11111111111111111111111111111111111111111111111111111111Ah�9Ah��Ah�/Ag�wAcC�ASoAF=qAE��AEXAE�AD�jAD  AA�A?�PA=�hA9�A7K�A4��A1��A01'A0��A0bA0�A1��A3�FA5/A5hsA++A��A!��A$1'A&�\A��A/A�-AXA��A
VAdZ@�=q@�33@ܼj@���@��@���@���@��@�\)@��F@{"�@q&�@h1'@^��@Vȴ@O�;@G+11111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B1'B-B�B�#B�B�PB�B�B�B|�Bx�BhsBdZBC�B
=B
�B
�B
�RB
��B
��B
��B
��B
�HB%B�B�B
��B	�B
v�B
ĜB%B
��B
�/B
��B
�^B
��B
�hB
jB
=qB

=B	�
B	��B	��B	�B	�PB	��B	�wB	��B	�TB
B
�B
1'B
A�B
^5B
z�11111111111111111111111111111111111111111111111111111111B.�B1 B.xB"nB��B.�B�!B�fB�sB�zB}�B{~Bk{Bf�BIUB�B
�,B
��B
�eB
�+B
��B
��B
ˇB
ޕB*BXB#EB
��B	�LB
v&B
��BaB
��B
�4B
�B
�B
�0B
��B
lmB
?�B
�B	��B	��B	��B	��B	�0B	�#B	��B	�UB	��B
�B
 B
1�B
A�B
^�B
z�11111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<��><��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     201704141605572017041416055720170414160557  AO  ARFM                                                                        20161003173515  IP                  G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20161003173515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20161003173515  QCF$                G�O�G�O�G�O�0               SI  ARCASIQCV2.1                                                                20170414160719  IP                  G�O�G�O�G�O�                SI  ARSQOW  V1.0ARGO_for_DMQC Climatology Version 2017V01                       20170414160719  IP                  G�O�G�O�G�O�                SI  ARDU                                                                        20170414160719  IP                  G�O�G�O�G�O�                SI  ARCAOW                                                                      20170414160752  IP                  G�O�G�O�G�O�                SI  ARDU                                                                        20180116160316  IP                  G�O�G�O�G�O�                SI  ARSQ                                                                        20180116160316  IP                  G�O�G�O�G�O�Timing          SI  ARDU                                                                        20180116160316  IP                  G�O�G�O�G�O�                