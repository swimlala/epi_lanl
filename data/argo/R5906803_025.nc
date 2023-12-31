CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-16T17:22:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̄   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230516172258  20230516172258  5906803 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      A   AO  9276                            2B  A   NAVIS_A                         1437                            170425                          863 @���g�1   @�`�v@;S�����dY��v�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A       @�ff@���A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�(�@\A�HA"�HAB�HAb�HA�p�A�p�A���A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CFzCH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D�D���D��D�E�DÅ�D���D��D�E�Dą�D���D��D�E�DŅ�D���D��D�E�Dƅ�D���D��D�E�Dǅ�D���D��D�E�Dȅ�D���D��D�E�DɅ�D���D��D�E�Dʅ�D���D��D�E�D˅�D���D��D�E�D̅�D���D��D�E�Dͅ�D���D��D�E�D΅�D���D��D�E�Dυ�D���D��D�E�DЅ�D���D��D�E�Dх�D���D��D�E�D҅�D���D��D�E�DӅ�D���D��D�E�Dԅ�D���D��D�E�DՅ�D���D��D�E�Dօ�D���D��D�E�Dׅ�D���D��D�E�D؅�D���D��D�E�Dم�D���D��D�E�Dڅ�D���D��D�E�Dۅ�D���D��D�E�D܅�D���D��D�E�D݅�D���D��D�E�Dޅ�D���D��D�E�D߅�D���D��D�E�D���D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D���D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D���D��D�E�D��D�D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�E�D���D���D��D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ȴA�A��^A���A���A�ƨA���A�ȴA�ȴA���A���A���A��
A��
A��
A��
A���A���A���A���A���A�A��9A��9A��9A��A���A���A��7A��A�n�A�p�A�r�A�p�A�n�A�l�A�jA�ffA�ffA�l�A�r�A�t�A�r�A�bNA�XA�Q�A�?}A�/A�"�A�1A�%A�A��A��#A�\)A�?}A�5?A�5?A�(�A���A��mA���A�bA���A�\)A��A�JA��\A�$�A���A�E�A���A�ZA�A��TA�"�A�n�A��
A���A��PA�ĜA�I�A��\A��jA��A�;dA��A��A��A�&�A�\)A��uA�jA��
A�jA��7A�ȴA���A�x�A�~�A�{A�E�A��A��;A�A�bNA�\)A��A�  A}�#A{��AzI�Axv�Av^5As��Ar��Ap��An�Al�/Al5?Ak�^Aj�HAiAiG�Ah�!Ag��Ag?}Af��Af�RAedZAd�uAb�A`��A_x�A^�yA^{A[O�AZ$�AW�-AUVATv�ARffAP��AP{AN�\AMAMS�AJ�AG?}AE�ADr�AC33ACAB��AB�AB��ABZAAƨA@~�A?�TA?A>=qA=��A=�wA=K�A<�\A;&�A9��A8�uA7�A7��A6�A6M�A5��A4�\A2n�A/A.I�A-�wA-G�A-"�A,�A,z�A+�A*jA)VA(VA'l�A%�A%hsA$bNA#x�A"�A"~�A!�hA ��A v�A ffA bNA M�A E�A 1A��A�A{A��AhsA�9A(�A��A�A�An�AE�A �A�7A�/A1A^5A?}A$�AĜA��A�A��AAVA�;A��A�7Ax�A
=A�+A�A�A
�A	XA�9A�AC�A��AjA�/A�A�RA$�AX@�t�@��@��;@�"�@�hs@��@�!@��@�ff@�-@���@��-@�p�@�G�@���@�bN@�@�S�@�!@�O�@���@�(�@�V@�9@�A�@�F@���@��@�V@ܓu@�(�@�l�@أ�@�S�@���@Դ9@ԓu@�C�@�V@���@�A�@��@�n�@��@���@ɺ^@�X@��/@�K�@�5?@��@�/@��`@�b@�@��-@��
@�33@�ȴ@�=q@��T@�/@� �@��@��@�O�@�/@���@��@��`@��/@��9@� �@���@��@�l�@���@��^@�`B@��@�j@�^5@�hs@�7L@��9@���@�bN@��@�t�@�C�@��@��+@��^@�bN@�
=@��H@��R@���@�~�@�E�@�{@��T@�O�@��@�1'@��P@�M�@�n�@��\@�ff@�@��^@��`@�  @�\)@�o@�ff@��T@��^@��7@�hs@�G�@��@��@�j@�t�@�;d@��@��@�=q@�O�@���@���@���@��9@��D@��D@�z�@�Z@�1@�
=@�/@�ȴ@�@��T@��T@��T@��#@��T@��-@��h@�x�@�?}@�V@��/@���@���@�l�@�K�@�33@�+@��!@��^@��h@�`B@�X@�?}@�?}@�7L@�&�@�&�@�&�@�&�@��@��@�%@��`@�Ĝ@�bN@�
=@��+@���@���@��u@��@�bN@�I�@�9X@�(�@�(�@�b@���@�ƨ@���@�+@��!@��@���@�x�@�x�@�?}@�Ĝ@��
@�S�@�@�ȴ@��R@��!@���@�~�@�^5@�-@�J@��#@���@�hs@�O�@�%@�r�@�(�@��@��;@���@���@���@���@�\)@��H@��+@�M�@�E�@��@���@��^@�`B@�7L@��@�%@���@�z�@�;@+@~��@~�y@~V@~5?@}�@}�-@}p�@|I�@z^5@z��@z��@zM�@yX@xbN@w�P@v�y@vv�@u��@u?}@t�/@t�@tI�@t9X@t9X@sƨ@r��@r^5@r^5@r^5@q��@q�#@qx�@qG�@p�u@p�@p�u@p�u@p�@p�@pr�@p �@p  @o��@n��@nE�@m�-@m`B@m�@l��@l�j@lj@lZ@l9X@k�F@k@j��@j^5@jJ@i�#@i�7@i%@hr�@hbN@hb@gl�@fȴ@f$�@e@e�@d��@dI�@c�F@c��@ct�@cC�@c@b�!@bM�@a�#@ax�@a%@`��@`�u@`  @_��@_�@_�P@_+@_+@^�y@^�R@^{@]�-@]��@]/@\�@\�j@\j@[��@[33@Z^5@Y��@X�`@X��@X�u@XbN@Xb@W�;@W�P@Wl�@W;d@V�@V�+@V5?@V@U@U`B@U?}@T�/@T�D@T(�@Sƨ@SS�@So@R�@R��@R��@R��@R-@RJ@Q�@Q��@P�u@P  @O�@O�P@O|�@O�@N��@N�y@N��@M�@LI�@L�@KdZ@J�!@J~�@J^5@I�^@I7L@I%@HĜ@HbN@G�;@G\)@G;d@G+@G�@F�y@F{@Ep�@D�@D�D@Dj@DI�@D9X@D(�@D1@D1@D1@C��@C�
@Cƨ@Cƨ@C�F@C�F@C��@CdZ@Co@B�!@B�\@B^5@A�#@@��@@A�@@b@@b@@  @?�P@?�@>��@>ff@>5?@>$�@>{@>@=�T@=�h@=/@<��@<��@<��@<Z@<1@;�m@;�m@;�m@;�m@;�
@;�F@;�@;dZ@;33@:�@:��@:�!@:�\@:^5@:J@9��@9hs@9X@9G�@9�@8�9@81'@7��@7�w@7��@7|�@7l�@6�@65?@6{@5@5`B@4�@4��@4z�@4Z@3��@3t�@3C�@3"�@3o@3@2�H@2��@2�\@2M�@2-@1�#@1�7@0��@0A�@0 �@/��@/�@.��@.$�@.@-�T@-��@-V@,��@,z�@,�@+��@+C�@+"�@+"�@*�H@*M�@)��@)��@)��@)�7@)x�@)%@(Q�@( �@'�;@'��@'K�@&��@&��@&E�@%��@%`B@%?}@%�@$�@$�/@$��@$�j@$�@$j@$�@#��@"�@!�^@!��@!�7@!X@!7L@!�@ ��@ �`@ ��@ Ĝ@ �u@ bN@ A�@  �@�@�w@��@�P@|�@|�@|�@l�@l�@\)@K�@K�@K�@+@�@�R@�R@�+@V@$�@{@�@@�h@�@�@��@j@Z@9X@��@ƨ@�@@��@�!@~�@=q@J@��@hs@��@�@Q�@1'@ �@  @  @�@�;@��@�@\)@+@�@
=@�y@�@ȴ@��@�+@{@��@/@�/@�@��@�D@j@I�@1@�
@��@�@dZ@33@�!@�\@n�@�@��@��@��@��@hs@hs@7L@&�@&�@�@%@�`@�9@r�@Q�@�w@l�@K�@+@
=@�@�R@��@��@�+@V@$�@{@@�T@�-@�h@�h@�@�@p�@`B@?}@�@�j@�D@9X@(�@1@ƨ@�F@�F@��@��@�@C�@@
��@
~�@
-@	��@	�@	�#@	��@	��@	��@	x�@	x�@	hs@	hs@	X@	X@	X@	X@	G�@	&�@��@�`@��@Ĝ@�9@�@Q�@b@�w@��@��@�P@|�@l�@\)@;d@+@��@�y@�y@�@ȴ@�R@��@V@$�@$�@{@@�@�@��@p�@�@�@�j@j@(�@��@��@dZ@C�@33@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ȴA�A��^A���A���A�ƨA���A�ȴA�ȴA���A���A���A��
A��
A��
A��
A���A���A���A���A���A�A��9A��9A��9A��A���A���A��7A��A�n�A�p�A�r�A�p�A�n�A�l�A�jA�ffA�ffA�l�A�r�A�t�A�r�A�bNA�XA�Q�A�?}A�/A�"�A�1A�%A�A��A��#A�\)A�?}A�5?A�5?A�(�A���A��mA���A�bA���A�\)A��A�JA��\A�$�A���A�E�A���A�ZA�A��TA�"�A�n�A��
A���A��PA�ĜA�I�A��\A��jA��A�;dA��A��A��A�&�A�\)A��uA�jA��
A�jA��7A�ȴA���A�x�A�~�A�{A�E�A��A��;A�A�bNA�\)A��A�  A}�#A{��AzI�Axv�Av^5As��Ar��Ap��An�Al�/Al5?Ak�^Aj�HAiAiG�Ah�!Ag��Ag?}Af��Af�RAedZAd�uAb�A`��A_x�A^�yA^{A[O�AZ$�AW�-AUVATv�ARffAP��AP{AN�\AMAMS�AJ�AG?}AE�ADr�AC33ACAB��AB�AB��ABZAAƨA@~�A?�TA?A>=qA=��A=�wA=K�A<�\A;&�A9��A8�uA7�A7��A6�A6M�A5��A4�\A2n�A/A.I�A-�wA-G�A-"�A,�A,z�A+�A*jA)VA(VA'l�A%�A%hsA$bNA#x�A"�A"~�A!�hA ��A v�A ffA bNA M�A E�A 1A��A�A{A��AhsA�9A(�A��A�A�An�AE�A �A�7A�/A1A^5A?}A$�AĜA��A�A��AAVA�;A��A�7Ax�A
=A�+A�A�A
�A	XA�9A�AC�A��AjA�/A�A�RA$�AX@�t�@��@��;@�"�@�hs@��@�!@��@�ff@�-@���@��-@�p�@�G�@���@�bN@�@�S�@�!@�O�@���@�(�@�V@�9@�A�@�F@���@��@�V@ܓu@�(�@�l�@أ�@�S�@���@Դ9@ԓu@�C�@�V@���@�A�@��@�n�@��@���@ɺ^@�X@��/@�K�@�5?@��@�/@��`@�b@�@��-@��
@�33@�ȴ@�=q@��T@�/@� �@��@��@�O�@�/@���@��@��`@��/@��9@� �@���@��@�l�@���@��^@�`B@��@�j@�^5@�hs@�7L@��9@���@�bN@��@�t�@�C�@��@��+@��^@�bN@�
=@��H@��R@���@�~�@�E�@�{@��T@�O�@��@�1'@��P@�M�@�n�@��\@�ff@�@��^@��`@�  @�\)@�o@�ff@��T@��^@��7@�hs@�G�@��@��@�j@�t�@�;d@��@��@�=q@�O�@���@���@���@��9@��D@��D@�z�@�Z@�1@�
=@�/@�ȴ@�@��T@��T@��T@��#@��T@��-@��h@�x�@�?}@�V@��/@���@���@�l�@�K�@�33@�+@��!@��^@��h@�`B@�X@�?}@�?}@�7L@�&�@�&�@�&�@�&�@��@��@�%@��`@�Ĝ@�bN@�
=@��+@���@���@��u@��@�bN@�I�@�9X@�(�@�(�@�b@���@�ƨ@���@�+@��!@��@���@�x�@�x�@�?}@�Ĝ@��
@�S�@�@�ȴ@��R@��!@���@�~�@�^5@�-@�J@��#@���@�hs@�O�@�%@�r�@�(�@��@��;@���@���@���@���@�\)@��H@��+@�M�@�E�@��@���@��^@�`B@�7L@��@�%@���@�z�@�;@+@~��@~�y@~V@~5?@}�@}�-@}p�@|I�@z^5@z��@z��@zM�@yX@xbN@w�P@v�y@vv�@u��@u?}@t�/@t�@tI�@t9X@t9X@sƨ@r��@r^5@r^5@r^5@q��@q�#@qx�@qG�@p�u@p�@p�u@p�u@p�@p�@pr�@p �@p  @o��@n��@nE�@m�-@m`B@m�@l��@l�j@lj@lZ@l9X@k�F@k@j��@j^5@jJ@i�#@i�7@i%@hr�@hbN@hb@gl�@fȴ@f$�@e@e�@d��@dI�@c�F@c��@ct�@cC�@c@b�!@bM�@a�#@ax�@a%@`��@`�u@`  @_��@_�@_�P@_+@_+@^�y@^�R@^{@]�-@]��@]/@\�@\�j@\j@[��@[33@Z^5@Y��@X�`@X��@X�u@XbN@Xb@W�;@W�P@Wl�@W;d@V�@V�+@V5?@V@U@U`B@U?}@T�/@T�D@T(�@Sƨ@SS�@So@R�@R��@R��@R��@R-@RJ@Q�@Q��@P�u@P  @O�@O�P@O|�@O�@N��@N�y@N��@M�@LI�@L�@KdZ@J�!@J~�@J^5@I�^@I7L@I%@HĜ@HbN@G�;@G\)@G;d@G+@G�@F�y@F{@Ep�@D�@D�D@Dj@DI�@D9X@D(�@D1@D1@D1@C��@C�
@Cƨ@Cƨ@C�F@C�F@C��@CdZ@Co@B�!@B�\@B^5@A�#@@��@@A�@@b@@b@@  @?�P@?�@>��@>ff@>5?@>$�@>{@>@=�T@=�h@=/@<��@<��@<��@<Z@<1@;�m@;�m@;�m@;�m@;�
@;�F@;�@;dZ@;33@:�@:��@:�!@:�\@:^5@:J@9��@9hs@9X@9G�@9�@8�9@81'@7��@7�w@7��@7|�@7l�@6�@65?@6{@5@5`B@4�@4��@4z�@4Z@3��@3t�@3C�@3"�@3o@3@2�H@2��@2�\@2M�@2-@1�#@1�7@0��@0A�@0 �@/��@/�@.��@.$�@.@-�T@-��@-V@,��@,z�@,�@+��@+C�@+"�@+"�@*�H@*M�@)��@)��@)��@)�7@)x�@)%@(Q�@( �@'�;@'��@'K�@&��@&��@&E�@%��@%`B@%?}@%�@$�@$�/@$��@$�j@$�@$j@$�@#��@"�@!�^@!��@!�7@!X@!7L@!�@ ��@ �`@ ��@ Ĝ@ �u@ bN@ A�@  �@�@�w@��@�P@|�@|�@|�@l�@l�@\)@K�@K�@K�@+@�@�R@�R@�+@V@$�@{@�@@�h@�@�@��@j@Z@9X@��@ƨ@�@@��@�!@~�@=q@J@��@hs@��@�@Q�@1'@ �@  @  @�@�;@��@�@\)@+@�@
=@�y@�@ȴ@��@�+@{@��@/@�/@�@��@�D@j@I�@1@�
@��@�@dZ@33@�!@�\@n�@�@��@��@��@��@hs@hs@7L@&�@&�@�@%@�`@�9@r�@Q�@�w@l�@K�@+@
=@�@�R@��@��@�+@V@$�@{@@�T@�-@�h@�h@�@�@p�@`B@?}@�@�j@�D@9X@(�@1@ƨ@�F@�F@��@��@�@C�@@
��@
~�@
-@	��@	�@	�#@	��@	��@	��@	x�@	x�@	hs@	hs@	X@	X@	X@	X@	G�@	&�@��@�`@��@Ĝ@�9@�@Q�@b@�w@��@��@�P@|�@l�@\)@;d@+@��@�y@�y@�@ȴ@�R@��@V@$�@$�@{@@�@�@��@p�@�@�@�j@j@(�@��@��@dZ@C�@33@@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BZB[#B[#B[#B[#BZB[#B[#BZBZBZBZBZBZB[#BZBZBZB[#B[#B[#B[#B\)B]/B]/B]/B]/B\)B]/B\)B\)B\)B]/B`BBbNBbNBbNBaHB`BB_;BbNBdZBdZBdZBe`Be`Be`Be`BdZBdZBdZBdZBcTBe`BdZBP�BL�BI�BI�BF�B7LBbB��B�mB�BB�
B�^B��B�1B`BBR�BI�B>wB"�B��B�B�B�B�)BɺB�dB�B��B��B�\Bw�BZBN�BF�B@�B8RB,B�BPBB
��B
�NB
ŢB
�?B
��B
��B
�bB
�B
q�B
bNB
XB
L�B
J�B
F�B
:^B
.B
!�B
�B
PB
B	��B	��B	�B	�`B	�/B	�5B	�#B	�
B	��B	��B	��B	ǮB	ÖB	��B	�wB	�FB	�!B	��B	��B	�VB	�=B	�+B	z�B	o�B	e`B	S�B	P�B	I�B	>wB	<jB	33B	-B	(�B	�B	DB��B��B��B��B��B�B�B�B�B�B�sB�`B�HB�;B�/B�#B�B��B��BȴBĜBÖBB��B�wB�XB�9B�B��B��B��B��B��B��B��B��B�oB�VB�=B�+B�B�B{�By�Bw�Bw�Bs�Br�Br�Br�Bq�Bq�Bq�Bo�Bo�Bk�BjBhsBffBdZBbNBbNB[#BZBYBXBW
BT�BR�BO�BK�BI�BF�BE�BD�BC�BB�BA�B@�B?}B>wB=qB=qB;dB;dB9XB8RB5?B49B33B1'B/B-B+B(�B$�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{BuBoBuB{B{B{BuBuB�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B'�B'�B)�B+B-B/B1'B49B6FB7LB9XB9XB<jB>wB?}BB�BE�BF�BG�BH�BH�BG�BH�BJ�BJ�BJ�BK�BK�BI�BH�BG�BI�BQ�BR�BS�BVBVBW
BYB^5B`BB`BBe`BgmBk�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bq�Bp�Bw�B|�B~�B�B�B�B�B�%B�1B�1B�=B�DB�JB�VB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�?B�?B�?B�LB�^B�^B�RB�XB�XB�^B��BǮBȴB��B��B��B��B��B��B��B�#B�;B�ZB�fB�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	JB	bB	�B	�B	�B	�B	 �B	!�B	"�B	#�B	#�B	#�B	$�B	$�B	%�B	(�B	-B	49B	6FB	8RB	8RB	9XB	>wB	E�B	H�B	I�B	K�B	L�B	M�B	N�B	P�B	Q�B	R�B	S�B	VB	XB	YB	ZB	[#B	`BB	bNB	bNB	dZB	e`B	ffB	e`B	e`B	gmB	jB	m�B	n�B	o�B	p�B	q�B	s�B	v�B	v�B	w�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�PB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�RB	�jB	�qB	�wB	�}B	��B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B

=B
DB
JB
JB
JB
PB
\B
\B
\B
\B
\B
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
6FB
7LB
7LB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
K�B
K�B
K�B
L�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
T�B
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
XB
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
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
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
aHB
aHB
aHB
aHB
bNB
bNB
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
dZB
dZB
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
k�B
k�B
k�B
k�B
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
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BZB[#B[#B[#B[#BZB[#B[#BZBZBZBZBZBZB[#BZBZBZB[#B[#B[#B[#B\)B]/B]/B]/B]/B\)B]/B\)B\)B\)B]/B`BBbNBbNBbNBaHB`BB_;BbNBdZBdZBdZBe`Be`Be`Be`BdZBdZBdZBdZBcTBe`BdZBP�BL�BI�BI�BF�B7LBbB��B�mB�BB�
B�^B��B�1B`BBR�BI�B>wB"�B��B�B�B�B�)BɺB�dB�B��B��B�\Bw�BZBN�BF�B@�B8RB,B�BPBB
��B
�NB
ŢB
�?B
��B
��B
�bB
�B
q�B
bNB
XB
L�B
J�B
F�B
:^B
.B
!�B
�B
PB
B	��B	��B	�B	�`B	�/B	�5B	�#B	�
B	��B	��B	��B	ǮB	ÖB	��B	�wB	�FB	�!B	��B	��B	�VB	�=B	�+B	z�B	o�B	e`B	S�B	P�B	I�B	>wB	<jB	33B	-B	(�B	�B	DB��B��B��B��B��B�B�B�B�B�B�sB�`B�HB�;B�/B�#B�B��B��BȴBĜBÖBB��B�wB�XB�9B�B��B��B��B��B��B��B��B��B�oB�VB�=B�+B�B�B{�By�Bw�Bw�Bs�Br�Br�Br�Bq�Bq�Bq�Bo�Bo�Bk�BjBhsBffBdZBbNBbNB[#BZBYBXBW
BT�BR�BO�BK�BI�BF�BE�BD�BC�BB�BA�B@�B?}B>wB=qB=qB;dB;dB9XB8RB5?B49B33B1'B/B-B+B(�B$�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{BuBoBuB{B{B{BuBuB�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#�B'�B'�B)�B+B-B/B1'B49B6FB7LB9XB9XB<jB>wB?}BB�BE�BF�BG�BH�BH�BG�BH�BJ�BJ�BJ�BK�BK�BI�BH�BG�BI�BQ�BR�BS�BVBVBW
BYB^5B`BB`BBe`BgmBk�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bq�Bp�Bw�B|�B~�B�B�B�B�B�%B�1B�1B�=B�DB�JB�VB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�?B�?B�?B�LB�^B�^B�RB�XB�XB�^B��BǮBȴB��B��B��B��B��B��B��B�#B�;B�ZB�fB�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	JB	bB	�B	�B	�B	�B	 �B	!�B	"�B	#�B	#�B	#�B	$�B	$�B	%�B	(�B	-B	49B	6FB	8RB	8RB	9XB	>wB	E�B	H�B	I�B	K�B	L�B	M�B	N�B	P�B	Q�B	R�B	S�B	VB	XB	YB	ZB	[#B	`BB	bNB	bNB	dZB	e`B	ffB	e`B	e`B	gmB	jB	m�B	n�B	o�B	p�B	q�B	s�B	v�B	v�B	w�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�PB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�LB	�RB	�jB	�qB	�wB	�}B	��B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�NB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B

=B
DB
JB
JB
JB
PB
\B
\B
\B
\B
\B
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
6FB
7LB
7LB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
K�B
K�B
K�B
L�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
T�B
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
XB
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
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
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
aHB
aHB
aHB
aHB
bNB
bNB
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
dZB
dZB
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
k�B
k�B
k�B
k�B
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
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�          "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              " l0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230516172258                                          AO  ARCAADJP                                                                    20230516172258    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230516172258  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230516172258  QCF$                G�O�G�O�G�O�0               