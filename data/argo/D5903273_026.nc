CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  8   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        F   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    =    FORMAT_VERSION                 	long_name         File format version    
_FillValue                    =0   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    =4   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    =8   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    =H   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    =X   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    =h   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  =�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  >@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  �  ?    CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        ?�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    ?�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    ?�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  `  ?�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    @8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    @D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  `  @H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  `  @�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  `  A   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    Ah   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           At   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    A�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            A�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           A�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           A�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    A�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    A�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    A�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        D�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    E    PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_CNDC_QC                	long_name         #Global quality flag of CNDC profile    conventions       Argo reference table 2a    
_FillValue                    E   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    E   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        &�  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  k�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     &�  uX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     &�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &�  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	�  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &�  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� #(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� ,�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� Sp   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� z   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� ��   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� �X   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     &� �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� ڠ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� @   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� 
�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	� 1�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     &� ;0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � a�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   b�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   z�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 ��Argo profile    3.1 1.2 19500101000000  20190219181637  20200831164726  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL                     AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @ի/7�@ի/7�@ի/7�111 @ի�Zb@ի�Zb@ի�Zb@6�p��
=@6�p��
=@6�p��
=�cK��v��cK��v��cK��v�111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                          ADA BDA  DA BDA @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D��D�J=D���D��fD�=D�O
D��
D��{D�3D�XRD���D���D�)D�B�Dڒ�D�D��
D�C�D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����    �L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�ͽ���    �L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ���    �L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�ͽ��ͽ��;L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L�;L��    �L�;L�;L�ͽ��;L�;L�;L�;L�;L�;L�;L�;L�ͽ��;L�;L�ͽ��ͽ��;L�;L�;L�ͽ���    ���;L�;L�ͽ���=��ͽ��;L�;L�;L�;L�ͽ��;L�ͽ��;L�;L�;L�ͽ��;L�ͽ��ͽ��ͽ��;L�;L�;L�;L�ͽ��;L�;L�;L�;L�ͽ���=���    �L�;L�;L�ͽ��;L��    �L�;L�;L�ͽ��ͽ��ͽ��ͽ��;L�;L�ͽ��;L�ͽ��ͽ��ͽ���    ���ͽ��;L�ͽ��ͽ��ͽ��ͽ���                    ���ͽ���        =���=���            ����                                    =���    =���=���=���=���=���=���    ����            =���    =���=���=���>L��    =���        ����            ����=���        =���>L��=���=���=���=���            ���;L��                =���=���=���                        =���    =���=���        ����                                ����=���            ���ͽ���                        ����            =���=���    =���    =���=���=���                =���            ����            =���=���    =���                            ����        =���    =���        =���=���=���        ����                =���        =���                            =���=���    ����        =���    ����            =���=���                        =���=���        ����                    =���                    =��ͽ���                =���=���=���=���    =���    =���                ����                =���                ����=���    =���            ���ͽ���            =���                =���                        =���                =���        =���=���        =���    ���ͽ��ͽ���        =���=���>L��>L��>���>���?   ?��?��?333?L��?L��?fff?�  ?���?���?���?�ff?�33?�  ?���?���?ٙ�?�ff?�33@   @ff@ff@��@33@��@   @&ff@,��@9��@@  @Fff@S33@Y��@`  @l��@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A��A��AffA	��A��AffA��A33A��A  A33A��A   A!��A$��A&ffA)��A+33A.ffA0  A333A4��A8  A9��A<��A>ffAA��AC33AD��AH  AI��AL��ANffAQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33AvffAx  A{33A|��A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Dp�fDp�3Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq33Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��@&ff@,��@9��@@  @Fff@S33@Y��@`  @l��@s33@y��@�  @�ff@���@�  @�33@�ff@���@�  @�33@���@���@�33@�ff@���@�  @�33@ə�@���@�33@�ff@ٙ�@�  @�33@陚@���@�33@�ff@���A   A��A��AffA	��A��AffA��A33A��A  A33A��A   A!��A$��A&ffA)��A+33A.ffA0  A333A4��A8  A9��A<��A>ffAA��AC33AD��AH  AI��AL��ANffAQ��AS33AVffAX  AY��A\��A^ffAa��Ac33AfffAh  Ak33Al��AnffAq��As33AvffAx  A{33A|��A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�  A���A���A�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A���A���A�ffA�  A���A�ffA�33A���A���A�33A�  A���A�ffA�33A�  A���A�ffA�  A���A�ffA�33A�  A���A�ffA�33A���A͙�A�ffA�  A���Aљ�A�33A�  A���A�ffA�33A�  A���A�ffA�33A�  A���Dp�fDp�3Dp��Dq  Dq�Dq3Dq�Dq&fDq,�Dq33Dq@ DqFfDqL�DqY�Dq` DqffDqs3Dqy�Dq� Dq��Dq�3Dq��Dq�fDq��Dq�3Dq� Dq�fDq��DqٚDq� Dq��Dq�3Dq��Dr  Dr�Dr3Dr  Dr&fDr,�Dr9�Dr@ DrFfDrS3DrY�Dr` Drl�Drs3Dr� Dr�fDr��Dr��Dr� Dr�fDr�3Dr��Dr� Dr��Dr�3Dr� Dr�fDr��Dr�3Ds  DsfDs3Ds�Ds  Ds,�Ds33Ds9�DsFfDsL�DsS3Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds��Ds�fDs��Ds�3Ds� Ds�fDs��DsٚDs� Ds�fDs�3Ds��Dt  Dt�Dt3Dt�Dt&fDt,�Dt33Dt@ DtFfDtL�DtY�Dt` DtffDts3Dty�Dt�fDt��Dt�3Dt� Dt�fDt��Dt��Dt� Dt�fDt�3DtٚDt� Dt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@:�G@��
@��
A�A!�AA�Aa�A���A���A���A���A���A���A�A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bx�GB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqHDq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�	�D�ND��fD��=D�D�R�D���D��RD�
D�\)D���D���D� D�FfDږfD��D���D�G�D�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<��	=�������������������<��	���������������������������������������������������������������������������������������������<��	���������<��	���<��	������������������������<��	=����������������������<��	���������������<��	������������<��	=����������<��	������������������������������������������<��	������������������<��	������������������������������<��	<��	���������<��	������������������������������������������=����������<��	������������������������<��	������<��	<��	���������<��	=�<��	������<��	>aG�<��	������������<��	���<��	���������<��	���<��	<��	<��	������������<��	������������<��	>aG�=����������<��	���=����������<��	<��	<��	<��	������<��	���<��	<��	<��	=�<��	<��	���<��	<��	<��	<��	=�=�=�=�=�<��	<��	=�=�>aG�>aG�=�=�=�<��	=�=�=�=�=�=�=�=�=�>aG�=�>aG�>aG�>aG�>aG�>aG�>aG�=�<��	=�=�=�>aG�=�>aG�>aG�>aG�>��
=�>aG�=�=�<��	=�=�=�<��	>aG�=�=�>aG�>��
>aG�>aG�>aG�>aG�=�=�=�<��	���=�=�=�=�>aG�>aG�>aG�=�=�=�=�=�=�>aG�=�>aG�>aG�=�=�<��	=�=�=�=�=�=�=�=�<��	>aG�=�=�=�<��	<��	=�=�=�=�=�=�<��	=�=�=�>aG�>aG�=�>aG�=�>aG�>aG�>aG�=�=�=�=�>aG�=�=�=�<��	=�=�=�>aG�>aG�=�>aG�=�=�=�=�=�=�=�<��	=�=�>aG�=�>aG�=�=�>aG�>aG�>aG�=�=�<��	=�=�=�=�>aG�=�=�>aG�=�=�=�=�=�=�=�>aG�>aG�=�<��	=�=�>aG�=�<��	=�=�=�>aG�>aG�=�=�=�=�=�=�>aG�>aG�=�=�<��	=�=�=�=�=�>aG�=�=�=�=�=�>aG�<��	=�=�=�=�>aG�>aG�>aG�>aG�=�>aG�=�>aG�=�=�=�=�<��	=�=�=�=�>aG�=�=�=�=�<��	>aG�=�>aG�=�=�=�<��	<��	=�=�=�>aG�=�=�=�=�>aG�=�=�=�=�=�=�>aG�=�=�=�=�>aG�=�=�>aG�>aG�=�=�>aG�=�<��	<��	<��	=�=�>aG�>aG�>��
>��
>�
>?�?�R?8Q�?8Q�?Q�?k�?k�?��\?�\)?�(�?���?���?�?\?�\)?�(�?�(�?���?�@G�@�@z@z@z�@�G@!G�@'�@.z@4z�@AG�@G�@Nz@Z�G@aG�@g�@tz�@z�G@���@��
@�=p@�p�@��
@�
=@�=p@���@��
@�
=@�p�@���@�
=@�=p@���@��
@�
=@�p�@У�@�
=@�=p@�p�@��
@�
=@�p�@��@�
=@�=pA Q�A�A�A�RAQ�A�A�RAQ�A�A�A�RA�A�A�RA!�A#�A&�RA(Q�A+�A-�A0Q�A1�A5�A6�RA9�A;�A>�RA@Q�AC�AE�AF�RAI�AK�AN�RAPQ�AS�AU�AXQ�AY�A[�A^�RA`Q�Ac�Ae�AhQ�Ai�Am�An�RApQ�As�Au�AxQ�Ay�A}�A~�RA���A�A�\)A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A���A�A��]A�(�A���A��]A�\)A���A�A�\)A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A�(�A�A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A�(�A���A]A�\)A���A�A�\)A�(�A���A�A�\)A�(�A�AΏ]A�\)A���A�Aҏ]A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�Dp�Dp��DqHDq�Dq{Dq�Dq!HDq.Dq4{Dq:�DqG�DqNDqT{DqaHDqg�DqnDqz�Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��DqǮDq�Dq�{Dq�HDq�Dq�{Dq��DrHDr�Dr{Dr�Dr'�Dr.Dr4{DrAHDrG�DrNDrZ�DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�Dr��Dr�HDrǮDr�{Dr��Dr�Dr�Dr�{Dr��Ds�DsDs�Ds!HDs'�Ds4{Ds:�DsAHDsNDsT{DsZ�Dsg�DsnDsz�Ds�HDs��Ds�{Ds��Ds�HDs�Ds�{Ds��DsǮDs�Ds�{Ds�HDs�Ds�Ds��DtHDt�Dt{Dt�Dt!HDt.Dt4{Dt:�DtG�DtNDtT{DtaHDtg�DtnDtz�Dt�HDt�Dt�{Dt��Dt��Dt�Dt�{Dt�HDtǮDt�Dt��Dt�HDt�Dt�{@.z@4z�@AG�@G�@Nz@Z�G@aG�@g�@tz�@z�G@���@��
@�=p@�p�@��
@�
=@�=p@���@��
@�
=@�p�@���@�
=@�=p@���@��
@�
=@�p�@У�@�
=@�=p@�p�@��
@�
=@�p�@��@�
=@�=pA Q�A�A�A�RAQ�A�A�RAQ�A�A�A�RA�A�A�RA!�A#�A&�RA(Q�A+�A-�A0Q�A1�A5�A6�RA9�A;�A>�RA@Q�AC�AE�AF�RAI�AK�AN�RAPQ�AS�AU�AXQ�AY�A[�A^�RA`Q�Ac�Ae�AhQ�Ai�Am�An�RApQ�As�Au�AxQ�Ay�A}�A~�RA���A�A�\)A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A���A�A��]A�(�A���A��]A�\)A���A�A�\)A�(�A���A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A�(�A�A��]A�\)A���A�A�\)A�(�A�A��]A�(�A���A�A�\)A�(�A���A]A�\)A���A�A�\)A�(�A���A�A�\)A�(�A�AΏ]A�\)A���A�Aҏ]A�(�A���A�A�\)A�(�A���A�A�\)A�(�A���A�Dp�Dp��DqHDq�Dq{Dq�Dq!HDq.Dq4{Dq:�DqG�DqNDqT{DqaHDqg�DqnDqz�Dq�HDq��Dq�{Dq��Dq�HDq�Dq�{Dq��DqǮDq�Dq�{Dq�HDq�Dq�{Dq��DrHDr�Dr{Dr�Dr'�Dr.Dr4{DrAHDrG�DrNDrZ�DraHDrg�Drt{Drz�Dr��Dr�Dr�{Dr�HDr��Dr�Dr��Dr�HDrǮDr�{Dr��Dr�Dr�Dr�{Dr��Ds�DsDs�Ds!HDs'�Ds4{Ds:�DsAHDsNDsT{DsZ�Dsg�DsnDsz�Ds�HDs��Ds�{Ds��Ds�HDs�Ds�{Ds��DsǮDs�Ds�{Ds�HDs�Ds�Ds��DtHDt�Dt{Dt�Dt!HDt.Dt4{Dt:�DtG�DtNDtT{DtaHDtg�DtnDtz�Dt�HDt�Dt�{Dt��Dt��Dt�Dt�{Dt�HDtǮDt�Dt��Dt�HDt�Dt�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϰ!AϮAϣ�A�n�A�AθRAΰ!AΩ�AΣ�AΟ�AΕ�AΑhAΉ7A�~�A�ffA̓A�&�A�  A���A̝�A�t�A��A�oA�VA�
=A��A�{A�v�A�hsA�p�A�jA�ZA�9XA���A˰!Aʴ9Aɴ9A��mA�l�A���A�I�AA�~�A��!A�JA��#A���A���A�Q�A��mA�ZA���A�C�A�ZA��A�
=A��A��A�p�A�1'A���A��A�+A���A�bA�VA�XA��A�^5A��
A��`A��A���A��uA�x�A�"�A�z�A�A�ĜA��A�VA��A��mA�A�A���A�M�A���A�oA��\A���A�ĜA�~�A�$�A��A���A�=qA��/A�|�A�oA�M�A�dZA�
=A�-A�-A�-A��\A��A�z�A�G�A�\)A��A��A���A�jA��A�?}A�n�A��A���A���A�|�A�1A�I�A��^A��A�A�A|ȴA{|�Aw�At(�AqO�Ao�AmƨAk
=Aj~�Ai��Ai?}Af��AeO�Ad=qAb1A^I�A\�\AYl�AW��AU%AT�AR=qAP��AN�AN1AL��AK?}AJ{AI%AHZAG�;AFE�AD�`ADbNAB�HA@��A>JA<�`A<�!A<�uA<n�A;��A:��A9�#A8ZA6��A6(�A5ƨA5�A3;dA2Q�A2(�A1�A1;dA0z�A/��A.M�A-��A-x�A,�RA+�mA*��A*1A)�FA)l�A(�HA'|�A&�+A%G�A${A#%A"bA!��A VAVA�mAI�A33A(�A�A�AA-A�A�A�uAA�AG�A�uAA�A�-AbNA-A�mA|�A�\AhsA	�TA	�-A	��A	��A	l�A��A��A�A�A-A��A%A�RAZA1'A �9@���@�=q@�p�@��9@���@���@��@�b@��@�-@��@�  @�@�@�p�@�z�@�(�@��@�\@���@旍@�z�@�o@�/@�1@ڰ!@��@���@�33@���@Լj@Դ9@�Q�@��@���@���@�bN@���@ύP@Η�@��@�l�@�E�@�?}@�b@�S�@�E�@Ĵ9@���@��@���@��y@�-@�Ĝ@��m@�K�@�E�@�hs@��@��y@�{@�hs@�Z@�dZ@�v�@���@�x�@��D@�A�@���@�C�@��@���@��+@���@�7L@��9@��@�Q�@� �@�1@�|�@��@���@���@�^5@�5?@�-@�J@�@��7@�?}@�7L@��@��u@��9@��@���@���@�v�@�5?@�V@��@���@��-@���@���@�l�@�;d@�
=@���@�ff@�^5@�ff@�^5@�ff@���@���@���@�@��#@��#@��T@���@���@���@��7@��@��/@�7L@��`@�&�@��`@���@���@�33@�E�@�hs@��D@��@��;@��u@���@�Ĝ@�j@��@� �@�A�@�bN@�r�@���@�\)@�K�@�+@�+@���@�V@�@�hs@���@�z�@��;@��w@��
@�  @��;@��@���@��@�(�@���@�\)@�=q@�-@�-@��7@�Q�@�C�@�5?@��/@�z�@�S�@�ff@�=q@�@���@�O�@��^@�@�p�@��@��9@�33@���@�n�@�ff@�^5@�n�@�v�@�~�@�n�@�M�@�@���@��^@��@�5?@��@�J@�@���@�{@�$�@��@�{@�@��@�@��^@��@�`B@�X@�G�@�%@�j@�(�@��m@��@�|�@�K�@�~�@��7@�1'@��@��R@��+@��@��@���@��@�"�@�33@�o@�
=@��H@���@�n�@��@���@��`@�!@{��@v�H@l�@`*�@X!@Q�@H�@B@<r�@92a@4'R@/C@+'�@$tT@��@g�@n/@>B@:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�5?A�^5A���A��A�9XA�A�A�VA�1'A���A���A��A�  A�9XA���A�A�ȴA�;dA�A�~�A��A�|�A���A��mA�r�A���A��`Aɡ�A�;dA�E�A��^A���A��DA��RA�A���A��hA��!A���A���A���A�G�A�n�A͗�A�ƨAɧ�A΋DA�bA�33A�z�A���A�1A�E�A���A���Aϥ�A�G�A���A��jA�ȴA�t�A�5?AöFA�33A��mA��A�;dA¸RA�1A�^5A�K�A�M�AƩ�A��A�`BA���A�S�A�JA���A��A�I�A��A�jA�33A��\A�/A���A�`BA�9XA�ƨA��-A��A�JAɇ+A�S�A�I�A��A��HA�&�A�S�A�%A��`Aϣ�A���A�x�A��mA�jA�33A�
=A�5?Aå�A���A�A�AϸRA�v�A��A�1A�{A�p�A���A��-A��TA���A���A�I�A���A���A�Q�A�VA�1'A�r�A���A���A�r�A�Q�A���A�;dA�-AÅA�7LA�ZA��TAʶFA�r�Aʰ!AϬA� �A�r�A�5?AϓuA�\)A���AʍPA�n�AϼjA���A�C�A�jA̅AϋDA�AƟ�A�l�A�9XAʼjA΍PA��`A϶FA���A��mAżjAυA�
=A�{A�Aϥ�A��A��Aɇ+A���A�A�p�A�hsA��AϺ^AϑhAϼjAϸRA�=qA�bNA�A���Aϩ�A�C�A���A�9XA�E�A�G�A��HA�jAϺ^A�7LA�x�A�  Aϟ�A˕�A�VA�K�Aϲ-AϸRA���AρAɓuAϩ�A͋DA���AϾwA�A���A�ĜA�Aϧ�AϾwA���AϼjAϾwAϾwAϼjAϸRA϶FA϶FAϮAϮAϰ!AϸRAϼjAϸRAϸRAϸRAϸRAϲ-AϼjAϺ^AϺ^AϼjAϼjAϺ^AϼjAϸRAϺ^AϼjAϺ^AϼjAϺ^AϼjAϾwA���A�ĜAϾwA�A���A�ȴAϾwAϼjAϸRA϶FAϾwAϺ^AϸRAϺ^AϼjAϺ^AϼjAϼjA�ƨA�ĜA���A���A�ƨA�ĜA�AϬA϶FAϲ-AϼjAϺ^A϶FAϺ^AϼjAϼjAϾwA���AϼjAϾwA�A�ƨA�ĜA�ĜA���A���A���A�ȴA���AϼjAϺ^AϸRAϼjAϼjAϺ^AϼjAϾwAϺ^AϺ^A���AϾwAϾwA�A�A���A���A���A�ƨAϼjA���A���AϼjAϼjAϾwAϾwAϾwAϾwAϾwAϾwAϼjAϼjA���A�A�ĜA���A�ƨA���A���A���A�ƨA���A�ƨA�ȴA�ȴA�ƨA�ȴAϾwA�ĜA�AϾwA�ĜAϾwAϾwAϾwA�ĜA���AϾwAϺ^AϺ^AϸRA���AϼjAϺ^AϾwAϼjAϺ^AϺ^AϺ^AϺ^AϸRAϸRAϸRAϸRAϸRA϶FAϸRA϶FAϸRAϸRAϼjA϶FAϴ9A϶FAϴ9A϶FAϸRAϾwAϺ^A���A�A�ĜAϾwA���AϼjAϾwA���AϸRAϼjAϲ-A϶FAϾwAϾwA���A�A���A���A�A���AϸRAϾwAϾwAϾwA�AϸRAϾwA���AϾwAϾwA���AϺ^A���AϺ^AϼjAϼjA�ĜAϺ^A�ƨA�AϸRA�ƨAϺ^A�AϸRA�A�A�A���A�ĜA���A�ĜA�A�ȴA���A�ĜA���A�ȴA�ƨA�ƨA���AϼjAϾwAϾwAϾwAϾwAϾwAϸRAϾwAϺ^AϸRAϺ^AϸRAϸRAϼjA�AϼjAϼjAϼjAϼjAϼjA϶FAϸRAϸRA϶FAϸRAϸRA϶FA϶FAϸRAϲ-A���A���A���AϾwAϼjAϸRA���AϼjAϺ^AϸRAϴ9A϶FAϸRA϶FAϴ9AϸRAϸRA϶FAϺ^AϺ^AϸRA���AϾwAϾwA�A���AϼjAϾwAϸRAϼjAϺ^AϸRAϴ9Aϴ9Aϴ9Aϰ!Aϰ!AϮAϧ�AϬAϬAϰ!AϮAϮAϲ-Aϰ!Aϩ�AϬAϬAϬAϩ�AϮAϬAϲ-Aϲ-Aϲ-Aϲ-Aϲ-Aϴ9Aϰ!Aϴ9A϶FAϸRA϶FA϶FA϶FAϮAϮAϮAϩ�Aϟ�Aϕ�Aϛ�Aϝ�Aϕ�Aϕ�Aϕ�Aϗ�Aϟ�Aϗ�Aϛ�A�ffA�p�A�JA�/A�9XA�I�A�K�A�M�A�l�A�dZA��A��A�JA�VA�{A�  A��A��;A��;A���A��A��A���A���AμjAμjAθRAκ^Aκ^AζFAζFAδ9AζFAδ9Aδ9Aβ-Aδ9AζFAβ-Aδ9Aβ-Aβ-Aΰ!Aβ-Aδ9Aβ-Aδ9Aΰ!Aΰ!Aΰ!Aΰ!Aΰ!Aΰ!AήAήAήAΩ�AΧ�AΧ�AΥ�AΧ�AΥ�AΧ�AΩ�AΩ�AΥ�AΥ�AΧ�AΧ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΣ�AΥ�AΥ�AΥ�AΣ�AΣ�AΡ�AΟ�AΝ�AΝ�AΝ�AΝ�AΝ�AΛ�AΛ�AΙ�AΙ�AΙ�AΙ�AΙ�AΗ�AΕ�AΗ�AΗ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΑhAΓuAΓuAΓuAΑhAΕ�AΓuAΓuAΓuAΓuAΓuAΓuAΓuAΑhAΕ�AΑhAΑhAΑhAΏ\AΉ7A΅A΅A΅A΅A΅A΅A΃A΅A΅A΅A΅A΃@��@�"�@��@��@�@�@�
=@�
=@�
=@�@�
=@�@�
=@�@�
=@�
=@�
=@�
=@�
=@�
=@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@�@�@�@�@��@��H@��@�ȴ@��R@��R@��!@��!@���@���@���@���@���@���@���@���@���@��\@��+@��+@��+@��+@��+@�~�@��+@�v�@�n�@�n�@�n�@�n�@�n�@�^5@�ff@�^5@�n�@�ff@�^5@�V@�^5@�^5@�^5@�M�@�-@�-@�$�@��@���@��@��T@��#@���@���@���@���@���@�@��^@��-@��-@���@��-@���@���@��h@��h@��@�p�@�p�@�`B@�O�@�?}@�&�@�V@��`@���@��@��u@��@�z�@�r�@�bN@�A�@�(�@� �@�b@���@��A϶FA϶FAϮAϥ�Aϧ�Aϧ�Aϩ�Aϧ�AϬAϩ�Aϲ-AϮAϮAϲ-Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9Aϲ-Aϲ-Aϴ9Aϲ-AϮAϬAϣ�Aϟ�Aϗ�Aϗ�Aϕ�Aϕ�Aϕ�AϓuAϓuAϕ�Aϟ�Aϗ�A�ZA�ffA�l�A�XA�I�A�E�A�O�A�jA�hsA�`BA�/A�{A�VA�oA�
=A���A��`A��`A��/A���A��mA��yA���A�ĜAκ^AμjAθRAθRAζFAζFAζFAδ9Aδ9Aβ-Aβ-Aβ-Aδ9Aβ-Aβ-Aβ-Aΰ!Aΰ!Aΰ!Aΰ!Aΰ!AήAήAΰ!Aΰ!AήAήAάAήAήAάAάAΩ�AΧ�AΧ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΣ�AΥ�AΣ�AΥ�AΥ�AΣ�AΣ�AΣ�AΣ�AΣ�AΣ�AΡ�AΡ�AΡ�AΡ�AΡ�AΟ�AΟ�AΝ�AΛ�AΝ�AΛ�AΛ�AΛ�AΙ�AΙ�AΙ�AΗ�AΗ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΑhAΑhAΏ\AΏ\AΏ\AΑhAΑhAΓuAΑhAΓuAΑhAΏ\AΑhAΑhAΏ\AΑhAΏ\AΏ\AΑhAΏ\AΏ\AΏ\AΏ\A΍PA·+A΃A΃A΃A΃A΃A΃A΃A΅A΃A΅A΃A΃@��@��@��@�o@�@�@�
=@�
=@�
=@�
=@�
=@�@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@��H@��@�ȴ@���@��R@��!@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@�v�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�ff@�n�@�n�@�^5@�^5@�ff@�ff@�V@�M�@�$�@�$�@�$�@��@�@��@��T@��T@��#@���@���@���@���@�@�@��-@��-@��-@���@���@���@��h@��h@��@�x�@�p�@�`B@�O�@�G�@�&�@�V@��@���@��@��u@��@�z�@�r�@�Z@�I�@�1'@�(�@�b@�  @��mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999Aϰ!AϮAϣ�A�n�A�AθRAΰ!AΩ�AΣ�AΟ�AΕ�AΑhAΉ7A�~�A�ffA̓A�&�A�  A���A̝�A�t�A��A�oA�VA�
=A��A�{A�v�A�hsA�p�A�jA�ZA�9XA���A˰!Aʴ9Aɴ9A��mA�l�A���A�I�AA�~�A��!A�JA��#A���A���A�Q�A��mA�ZA���A�C�A�ZA��A�
=A��A��A�p�A�1'A���A��A�+A���A�bA�VA�XA��A�^5A��
A��`A��A���A��uA�x�A�"�A�z�A�A�ĜA��A�VA��A��mA�A�A���A�M�A���A�oA��\A���A�ĜA�~�A�$�A��A���A�=qA��/A�|�A�oA�M�A�dZA�
=A�-A�-A�-A��\A��A�z�A�G�A�\)A��A��A���A�jA��A�?}A�n�A��A���A���A�|�A�1A�I�A��^A��A�A�A|ȴA{|�Aw�At(�AqO�Ao�AmƨAk
=Aj~�Ai��Ai?}Af��AeO�Ad=qAb1A^I�A\�\AYl�AW��AU%AT�AR=qAP��AN�AN1AL��AK?}AJ{AI%AHZAG�;AFE�AD�`ADbNAB�HA@��A>JA<�`A<�!A<�uA<n�A;��A:��A9�#A8ZA6��A6(�A5ƨA5�A3;dA2Q�A2(�A1�A1;dA0z�A/��A.M�A-��A-x�A,�RA+�mA*��A*1A)�FA)l�A(�HA'|�A&�+A%G�A${A#%A"bA!��A VAVA�mAI�A33A(�A�A�AA-A�A�A�uAA�AG�A�uAA�A�-AbNA-A�mA|�A�\AhsA	�TA	�-A	��A	��A	l�A��A��A�A�A-A��A%A�RAZA1'A �9@���@�=q@�p�@��9@���@���@��@�b@��@�-@��@�  @�@�@�p�@�z�@�(�@��@�\@���@旍@�z�@�o@�/@�1@ڰ!@��@���@�33@���@Լj@Դ9@�Q�@��@���@���@�bN@���@ύP@Η�@��@�l�@�E�@�?}@�b@�S�@�E�@Ĵ9@���@��@���@��y@�-@�Ĝ@��m@�K�@�E�@�hs@��@��y@�{@�hs@�Z@�dZ@�v�@���@�x�@��D@�A�@���@�C�@��@���@��+@���@�7L@��9@��@�Q�@� �@�1@�|�@��@���@���@�^5@�5?@�-@�J@�@��7@�?}@�7L@��@��u@��9@��@���@���@�v�@�5?@�V@��@���@��-@���@���@�l�@�;d@�
=@���@�ff@�^5@�ff@�^5@�ff@���@���@���@�@��#@��#@��T@���@���@���@��7@��@��/@�7L@��`@�&�@��`@���@���@�33@�E�@�hs@��D@��@��;@��u@���@�Ĝ@�j@��@� �@�A�@�bN@�r�@���@�\)@�K�@�+@�+@���@�V@�@�hs@���@�z�@��;@��w@��
@�  @��;@��@���@��@�(�@���@�\)@�=q@�-@�-@��7@�Q�@�C�@�5?@��/@�z�@�S�@�ff@�=q@�@���@�O�@��^@�@�p�@��@��9@�33@���@�n�@�ff@�^5@�n�@�v�@�~�@�n�@�M�@�@���@��^@��@�5?@��@�J@�@���@�{@�$�@��@�{@�@��@�@��^@��@�`B@�X@�G�@�%@�j@�(�@��m@��@�|�@�K�@�~�@��7@�1'@��@��R@��+@��@��@���@��@�"�@�33@�o@�
=@��H@���@�n�@��@���G�O�@�!@{��@v�H@l�@`*�@X!@Q�@H�@B@<r�@92a@4'R@/C@+'�@$tT@��@g�@n/@>B@:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�5?A�^5A���A��A�9XA�A�A�VA�1'A���A���A��A�  A�9XA���A�A�ȴA�;dA�A�~�A��A�|�A���A��mA�r�A���A��`Aɡ�A�;dA�E�A��^A���A��DA��RA�A���A��hA��!A���A���A���A�G�A�n�A͗�A�ƨAɧ�A΋DA�bA�33A�z�A���A�1A�E�A���A���Aϥ�A�G�A���A��jA�ȴA�t�A�5?AöFA�33A��mA��A�;dA¸RA�1A�^5A�K�A�M�AƩ�A��A�`BA���A�S�A�JA���A��A�I�A��A�jA�33A��\A�/A���A�`BA�9XA�ƨA��-A��A�JAɇ+A�S�A�I�A��A��HA�&�A�S�A�%A��`Aϣ�A���A�x�A��mA�jA�33A�
=A�5?Aå�A���A�A�AϸRA�v�A��A�1A�{A�p�A���A��-A��TA���A���A�I�A���A���A�Q�A�VA�1'A�r�A���A���A�r�A�Q�A���A�;dA�-AÅA�7LA�ZA��TAʶFA�r�Aʰ!AϬA� �A�r�A�5?AϓuA�\)A���AʍPA�n�AϼjA���A�C�A�jA̅AϋDA�AƟ�A�l�A�9XAʼjA΍PA��`A϶FA���A��mAżjAυA�
=A�{A�Aϥ�A��A��Aɇ+A���A�A�p�A�hsA��AϺ^AϑhAϼjAϸRA�=qA�bNA�A���Aϩ�A�C�A���A�9XA�E�A�G�A��HA�jAϺ^A�7LA�x�A�  Aϟ�A˕�A�VA�K�Aϲ-AϸRA���AρAɓuAϩ�A͋DA���AϾwA�A���A�ĜA�Aϧ�AϾwA���AϼjAϾwAϾwAϼjAϸRA϶FA϶FAϮAϮAϰ!AϸRAϼjAϸRAϸRAϸRAϸRAϲ-AϼjAϺ^AϺ^AϼjAϼjAϺ^AϼjAϸRAϺ^AϼjAϺ^AϼjAϺ^AϼjAϾwA���A�ĜAϾwA�A���A�ȴAϾwAϼjAϸRA϶FAϾwAϺ^AϸRAϺ^AϼjAϺ^AϼjAϼjA�ƨA�ĜA���A���A�ƨA�ĜA�AϬA϶FAϲ-AϼjAϺ^A϶FAϺ^AϼjAϼjAϾwA���AϼjAϾwA�A�ƨA�ĜA�ĜA���A���A���A�ȴA���AϼjAϺ^AϸRAϼjAϼjAϺ^AϼjAϾwAϺ^AϺ^A���AϾwAϾwA�A�A���A���A���A�ƨAϼjA���A���AϼjAϼjAϾwAϾwAϾwAϾwAϾwAϾwAϼjAϼjA���A�A�ĜA���A�ƨA���A���A���A�ƨA���A�ƨA�ȴA�ȴA�ƨA�ȴAϾwA�ĜA�AϾwA�ĜAϾwAϾwAϾwA�ĜA���AϾwAϺ^AϺ^AϸRA���AϼjAϺ^AϾwAϼjAϺ^AϺ^AϺ^AϺ^AϸRAϸRAϸRAϸRAϸRA϶FAϸRA϶FAϸRAϸRAϼjA϶FAϴ9A϶FAϴ9A϶FAϸRAϾwAϺ^A���A�A�ĜAϾwA���AϼjAϾwA���AϸRAϼjAϲ-A϶FAϾwAϾwA���A�A���A���A�A���AϸRAϾwAϾwAϾwA�AϸRAϾwA���AϾwAϾwA���AϺ^A���AϺ^AϼjAϼjA�ĜAϺ^A�ƨA�AϸRA�ƨAϺ^A�AϸRA�A�A�A���A�ĜA���A�ĜA�A�ȴA���A�ĜA���A�ȴA�ƨA�ƨA���AϼjAϾwAϾwAϾwAϾwAϾwAϸRAϾwAϺ^AϸRAϺ^AϸRAϸRAϼjA�AϼjAϼjAϼjAϼjAϼjA϶FAϸRAϸRA϶FAϸRAϸRA϶FA϶FAϸRAϲ-A���A���A���AϾwAϼjAϸRA���AϼjAϺ^AϸRAϴ9A϶FAϸRA϶FAϴ9AϸRAϸRA϶FAϺ^AϺ^AϸRA���AϾwAϾwA�A���AϼjAϾwAϸRAϼjAϺ^AϸRAϴ9Aϴ9Aϴ9Aϰ!Aϰ!AϮAϧ�AϬAϬAϰ!AϮAϮA϶FA϶FAϮAϥ�Aϧ�Aϧ�Aϩ�Aϧ�AϬAϩ�Aϲ-AϮAϮAϲ-Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9Aϲ-Aϲ-Aϴ9Aϲ-AϮAϬAϣ�Aϟ�Aϗ�Aϗ�Aϕ�Aϕ�Aϕ�AϓuAϓuAϕ�Aϟ�Aϗ�A�ZA�ffA�l�A�XA�I�A�E�A�O�A�jA�hsA�`BA�/A�{A�VA�oA�
=A���A��`A��`A��/A���A��mA��yA���A�ĜAκ^AμjAθRAθRAζFAζFAζFAδ9Aδ9Aβ-Aβ-Aβ-Aδ9Aβ-Aβ-Aβ-Aΰ!Aΰ!Aΰ!Aΰ!Aΰ!AήAήAΰ!Aΰ!AήAήAάAήAήAάAάAΩ�AΧ�AΧ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΣ�AΥ�AΣ�AΥ�AΥ�AΣ�AΣ�AΣ�AΣ�AΣ�AΣ�AΡ�AΡ�AΡ�AΡ�AΡ�AΟ�AΟ�AΝ�AΛ�AΝ�AΛ�AΛ�AΛ�AΙ�AΙ�AΙ�AΗ�AΗ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΑhAΑhAΏ\AΏ\AΏ\AΑhAΑhAΓuAΑhAΓuAΑhAΏ\AΑhAΑhAΏ\AΑhAΏ\AΏ\AΑhAΏ\AΏ\AΏ\AΏ\A΍PA·+A΃A΃A΃A΃A΃A΃A΃A΅A΃A΅A΃A΃@��@��@��@�o@�@�@�
=@�
=@�
=@�
=@�
=@�@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@��H@��@�ȴ@���@��R@��!@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@�v�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�ff@�n�@�n�@�^5@�^5@�ff@�ff@�V@�M�@�$�@�$�@�$�@��@�@��@��T@��T@��#@���@���@���@���@�@�@��-@��-@��-@���@���@���@��h@��h@��@�x�@�p�@�`B@�O�@�G�@�&�@�V@��@���@��@��u@��@�z�@�r�@�Z@�I�@�1'@�(�@�b@�  @��mA϶FA϶FAϮAϥ�Aϧ�Aϧ�Aϩ�Aϧ�AϬAϩ�Aϲ-AϮAϮAϲ-Aϲ-Aϰ!Aϰ!Aϲ-Aϴ9Aϲ-Aϲ-Aϴ9Aϲ-AϮAϬAϣ�Aϟ�Aϗ�Aϗ�Aϕ�Aϕ�Aϕ�AϓuAϓuAϕ�Aϟ�Aϗ�A�ZA�ffA�l�A�XA�I�A�E�A�O�A�jA�hsA�`BA�/A�{A�VA�oA�
=A���A��`A��`A��/A���A��mA��yA���A�ĜAκ^AμjAθRAθRAζFAζFAζFAδ9Aδ9Aβ-Aβ-Aβ-Aδ9Aβ-Aβ-Aβ-Aΰ!Aΰ!Aΰ!Aΰ!Aΰ!AήAήAΰ!Aΰ!AήAήAάAήAήAάAάAΩ�AΧ�AΧ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΥ�AΣ�AΥ�AΣ�AΥ�AΥ�AΣ�AΣ�AΣ�AΣ�AΣ�AΣ�AΡ�AΡ�AΡ�AΡ�AΡ�AΟ�AΟ�AΝ�AΛ�AΝ�AΛ�AΛ�AΛ�AΙ�AΙ�AΙ�AΗ�AΗ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΕ�AΑhAΑhAΏ\AΏ\AΏ\AΑhAΑhAΓuAΑhAΓuAΑhAΏ\AΑhAΑhAΏ\AΑhAΏ\AΏ\AΑhAΏ\AΏ\AΏ\AΏ\A΍PA·+A΃A΃A΃A΃A΃A΃A΃A΅A΃A΅A΃A΃@��@��@��@�o@�@�@�
=@�
=@�
=@�
=@�
=@�@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�o@�
=@�
=@�
=@�
=@�@�@�@���@���@��H@��@�ȴ@���@��R@��!@���@���@���@���@���@���@���@��\@��\@��\@��\@��\@��+@��+@��+@��+@��+@��+@�v�@�n�@�n�@�n�@�n�@�n�@�ff@�ff@�ff@�n�@�n�@�^5@�^5@�ff@�ff@�V@�M�@�$�@�$�@�$�@��@�@��@��T@��T@��#@���@���@���@���@�@�@��-@��-@��-@���@���@���@��h@��h@��@�x�@�p�@�`B@�O�@�G�@�&�@�V@��@���@��@��u@��@�z�@�r�@�Z@�I�@�1'@�(�@�b@�  @��mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�S�@�H,=�=�?��@�U�=�|>pP@R�@�R~=}�^=�
=�pz>��=�y=���=��=��O=�=�6�=�^�?dE=���>��>I�m=��=�J�=���>=Y@�N�=��=vN=�)�=�n�=��s=��?�t=�w\=�eV=�2>�r?�`�=�	W>�?V�r@�|>i,�@�Ec=�_[>��=Y�M=��=�g�=�>&�>�	�@�F@��=�5=ԕ?��z=���=�A�>�@^�4@�K�=��9>#�f?�	�?I��>\@�@�&=�/Z@*�y=�E�>Bb�@�I�@�PH=Դ�=��8>Þ�@�M@=��a=�J�=�=G=���=��>=�k@$��>��?���>	k�=�ŗ=�;>z:@�֡@F�r=�
�=��Z>g-�@g�>d�@��@�V�?��>UG@s��=���?��a=�W?=���>%�@n�	>:�H@�P�@�Uq=�k>��@5��@�\�=�Bp=��u>��E>�q�=�@�=�k�?̡�=yrG=�&l=�5??`�=��>A @�Z2@�A�=��>�(�@�?�@Jƽ=�?U9�?�Q?w[�?�s?dNQ>& @�.=�l�>L@Q$t@��%@#�=���>"�U@�Y`@�U@�\>]9�=�!->|Hk@�+k@�Uq?|Hk>�?f�w=�n?�ם>��~@�[W=��=�+>�u�@�QD>W ?��@�Y�@�XO@H�=���>�g@(8\@�Y�@�[W> ��>r�z@�Xd@��^@�Z�@�Z�?!l@�j@=�?Jw@�Y!@<U�@�X�=���>��@YF�@�W�@�W�@�V�>3 �@�W�@2v@�S�>:
R@qD�@�Ta@�S�@�S�@�Z2@�[B>h`W@�Y�@�P@�Z2@�Z�@�Z�@�Z2@�Z@�Z2@�Y!@�X�@�XO@�Y!@�Z2@�Z@�Y�@�X�@�X�@�W�@�U@�V�@�Y�@�Y�@�Y�@�Y�@�X�@�Y@�Xd@�X�@�Z@�Z�@�Z@�Y�@�Y�@�Z@�[W@�Z2@�Z�@�Z2@�Xd@�Y�@�Y�@�Z2@�Y�@�Y�@�Y�@�Y�@�Y!@�Y!@�Y@�Y�@�Y�@�Y`@�X@�Y�@�Y�@�Y�@�Y@�Y�@�Y�@�X�@�Z�@�Y�@�Y@�Xd@�X�@�Y@�Y�@�Y!@�W?@�X�@�Xd@�Y@�Y!@�XO@�Z@�Y�@�X�@�Y�@�Z�@�Y`@�Y!@�Z2@�Y`@�Y!@�Y!@�Z�@�Y�@�Y�@�Y@�Xd@�Xd@�Z@�Y�@�Y�@�Y�@�Y�@�Z@�Z�@�Z2@�Y�@�[W@�Z�@�Z�@�Z�@�[W@�[B@�Z2@�Z�@�Z�@�Z�@�Z2@�[�@�Z�@�Z�@�[�@�\}@�[�@�[�@�[�@�[B@�Y�@�Z2@�[�@�\�@�Z�@�Z�@�Z2@�Z�@�[B@�Y�@�[W@�[�@�[�@�\}@�[�@�\}@�[B@�[�@�Z�@�Z�@�Z�@�\}@�Z�@�[�@�Z2@�Z@�Z2@�Y!@�Y@�X�@�Z2@�Y�@�Y!@�X�@�Y`@�Xd@�X@�XO@�Xd@�X�@�Y@�X@�W�@�X@�Xd@�W?@�Xd@�Xd@�W�@�X@�X�@�W�@�Y!@�X�@�XO@�W�@�Y�@�X�@�Xd@�X�@�Y@�X�@�X�@�Y`@�Y�@�Y�@�Y!@�X�@�X�@�X@�X@�Y@�Xd@�X@�Y�@�Y!@�X@�W�@�Xd@�X�@�X�@�Y!@�Y�@�Y@�X@�W�@�W�@�WT@�W�@�W�@�WT@�V@�V�@�V.@�XO@�W�@�U�@�V�@�W�@�X@�W�@�Y!@�Z2@�XO@�X@�WT@�XO@�Xd@�X�@�Y�@�Y�@�W�@�Y�@�Y�@�Z�@�Y�@�Y�@�Z@�Y�@�Z�@�Z@�Y�@�Y!@�Y`@�Y�@�X�@�XO@�X�@�X�@�W�@�Y!@�Y@�XO@�X�@�XO@�Y!@�Y!@�Y@�X�@�X@�Y@�Xd@�X@�X�@�W�@�W�@�W�@�Xd@�Xd@�XO@�X�@�Xd@�W�@�X@�W�@�W�@�Xd@�W?@�V�@�T�@�V�@�W�@�W�@�W?@�Xd@�WT@�Y@�Y!@�Z2@�Z@�Z�@�[W@�[�@�[W@�[�@�[�@�[W@�Y`@�Y`@�Y�@�Y`@�X@�V�@�U@�U@�U@�R�@�R�@�R�@�U@�Uq@�Ta@�S�@�Uq@�W @�U�@�R�@�R�@�S�@�SP@�S�@�T�@�U@�U�@�U�@�U�@�Uq@�VC@�Uq@�U2@�Uq@�VC@�V�@�U�@�U�@�S�@�R�@�P�@�P�@�J#@�J�@�If@�J#@�H�@�F�@�F�@�F�@�H�@�I@�F5@�C�@�7v@��@�H@�$t@� @�$ @�&�@�+�@�,@�~@��@��@��@��@��@��r@���@��D@��]@���@��L@��@��@��@���@��@���@��l@��l@��[@��@���@��F@���@��F@��[@��[@��@���@���@��K@��@��@��@���@��K@��@���@���@��@��@���@��@���@���@��}@��X@���@��@��@��@��@��X@���@��X@��m@��m@��m@��@��@��@��@��m@��m@��@��@��@��@��@��\@��\@���@��@��6@���@��z@��&@���@���@��i@���@��@��@��@��X@��H@��@��X@���@���@��H@���@���@���@��"@��7@���@��z@���@��7@��7@���@���@��z@��z@��z@��&@��@���@��@��@��@��@���@��j@��Y@�ی@�٩@�پ@��j@�٩@�پ@�٩@�٩@��@@�٩@�٩@��@@��@��j@Qj@Qj@@Qj@Qhs@Qg�@QiD@Qj@Qj@@Qj@Qj@@Qj@@Qj�@Qj�@Qk@Qkf@Qk�@Qk�@Qk�@Qk�@Ql@Ql@Ql@Qla@Ql@Qla@Ql@Qk�@Qk�@Qkf@Qj�@Qj�@Qi�@Qi@Qf�@Qd�@Qc^@Qa�@Q`�@Q_�@Q_@Q^t@Q^J@Q]y@Q\�@Q\)@Q[�@Q[�@Q[�@Q[�@QZ�@QZ\@QZ\@QZ�@QY�@QY�@QY�@QY�@QX�@QVC@QVC@QVC@QU�@QUG@QUG@QT�@QT�@QT�@QU�@QTL@QS�@QR�@QR�@QQY@QO�@QJ#@QH�@QHV@QF�@QC@Q?�@Q=�@Q=@Q;@Q:@Q:@Q9�@Q9@Q8@Q6z@Q4�@Q3�@Q2�@Q28@Q0�@Q-�@Q,�@Q+V@Q)_@Q&�@Q$�@Q!�@Q�@Qm@Q/@Q�@Q_@Q�@Q\@P��@P�@P�b@P�<@P�s@P�Z@P��@P�t@P��@P�6@P�@P�@�{_@�z�@�x�@�t @�t~@�t�@�uO@�t�@�v�@�t�@�y�@�wp@�v�@�y)@�y>@�xl@�x-@�y�@�y�@�zx@�y}@�y�@�yh@�w�@�vu@�q�@�q7@�m�@�m@�la@�k�@�l�@�k'@�l@�k�@�oT@�pz@�U�@�W�@�\)@�X�@�K�@�H�@�M�@�X�@�X�@�U@�E9@�4�@�1@�3H@�/�@�) @� �@�!l@��@��@�!�@�#:@��@��@��@��@��@�@�A@��@�@��@��@�J@�_@�J@��@��@�J@��@�N@�9@�
�@�9@�N@�
�@�N@�N@�$@�
�@�
�@�
�@�
�@�
�@�
R@�	�@�	l@��@�@�t@�t@��@�t@��@�F@�F@�@�@�@��@��@��@��@�1@�1@�1@�@��@��@�J@��@�t@��@�y@�y@�%@�S@��@��@��@�l@�@��@��@�q@�q@�u@�2@��@�@��@��@�u@� ~@� T@� @� @� �@� T@� ?@�;@�;@�e@� �@� T@� �@� ~@� ~@� T@� ~@� �@� i@� ~@� �@� T@� @���@��7@���@���@���@���@���@���@���@���@���@���@���@���@Q��@Q��@Q��@Q�I@Q��@Q��@Q�w@Q��@Q�s@Q�s@Q�s@Q��@Q��@Q��@Q�D@Q��@Q�@Q�@Q�@@Q�@Q�@Q�@@Q��@Q�@Q�<@Q�f@Q��@Q��@Q��@Q�@@Q�@@Q��@Q�n@Q�s@Q�V@Q��@Q@Q}�@Q|F@Q{ @Qzx@Qy�@Qy}@Qy)@Qx@Qw�@Qw\@Qw@Qw2@Qw2@Qv�@Qv@Qu�@Qu�@Qu�@Qud@Qu�@Qud@Qt?@Qq�@Qq�@Qq�@Qr@Qq�@Qp�@Qp�@Qp�@Qr@Qq�@Qo*@QpP@Qpz@Qp�@Qn�@Ql�@Qf'@Qe�@Qe�@Qc�@Q`B@Q]O@QZ@QY�@QW@QV�@QV�@QV�@QVm@QT�@QS�@QR*@QP�@QP�@QOa@QO7@QK�@QK@QI{@QH@QE�@QDg@QA�@Q?)@Q;�@Q6�@Q2a@Q*�@Q%F@Q \@QL@Q�@Q3@Q�@Q�@Q�@Q	@QJ@QW@Q@P�&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        3344344434444444444444444444344444444444444434344444444334444443344444344443344434444444444444344443443443444443433444344444444444443344344444444344334443334443344444434443443344443344333343443434433334343433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�S�@�H.G�O�G�O�@�U�G�O�G�O�G�O�@�R~G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�zG�O�@�EfG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�F@��G�O�G�O�G�O�G�O�G�O�G�O�@^�:@�K�G�O�G�O�G�O�G�O�G�O�@�*G�O�G�O�G�O�G�O�@�I�@�PJG�O�G�O�G�O�@�M<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�֦G�O�G�O�G�O�G�O�@g�G�O�G�O�@�V�G�O�G�O�@s��G�O�G�O�G�O�G�O�G�O�@n�
G�O�@�P�@�UrG�O�G�O�G�O�@�\�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�Z3@�A�G�O�G�O�@�?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�.G�O�G�O�@Q$r@��%G�O�G�O�G�O�@�Yc@�U @�\G�O�G�O�G�O�@�+l@�UrG�O�G�O�G�O�G�O�G�O�G�O�@�[ZG�O�G�O�G�O�@�QEG�O�G�O�@�Y�@�XSG�O�G�O�G�O�G�O�@�Y�@�[ZG�O�G�O�@�Xf@��\@�Z�@�Z�G�O�@�jBG�O�G�O�@�Y#G�O�@�X�G�O�G�O�@YF�@�W�@�X @�V�G�O�@�W�G�O�@�S�G�O�@qD�@�Tb@�S�@�S�@�Z,@�[DG�O�@�Y�G�O�@�Z,@�Z�@�Z�@�Z2@�Z@�Z/@�Y @�X�@�XN@�Y!@�Z8@�Z@�Y�@�X�@�X�@�W�@�U@�V�@�Y�@�Y�@�Y�@�Y�@�X�@�Y@�Xb@�X�@�Z@�Z�@�Z@�Y�@�Y�@�Z@�[W@�Z3@�Z�@�Z2@�Xc@�Y�@�Y�@�Z2@�Y�@�Y�@�Y�@�Y�@�Y"@�Y@�Y@�Y�@�Y�@�Yb@�X@�Y�@�Y�@�Y�@�Y@�Y�@�Y�@�X�@�Z�@�Y�@�Y@�Xa@�X�@�Y@�Y�@�Y@�W?@�X�@�Xc@�Y@�Y#@�XP@�Z@�Y�@�X�@�Y�@�Z�@�Y^@�Y@�Z/@�Yb@�Y"@�Y!@�Z�@�Y�@�Y�@�Y@�Xe@�X^@�Z@�Y�@�Y�@�Y�@�Y�@�Z@�Z�@�Z5@�Y�@�[W@�Z�@�Z�@�Z�@�[T@�[A@�Z3@�Z�@�Z�@�Z�@�Z/@�[�@�Z�@�Z�@�[�@�\@�[�@�[�@�[�@�[B@�Y�@�Z2@�[�@�\�@�Z�@�Z�@�Z,@�Z�@�[F@�Y�@�[Y@�[�@�[�@�\x@�[�@�\{@�[@@�[�@�Z�@�Z�@�Z�@�\~@�Z�@�[�@�Z8@�Z@�Z,@�Y@�Y@�X�@�Z6@�Y�@�Y!@�X�@�Y`@�Xd@�X@�XS@�Xc@�X�@�Y@�X@�W�@�X@�Xb@�WB@�Xb@�Xf@�W�@�X@�X�@�W�@�Y@�X�@�XQ@�W�@�Y�@�X�@�Xg@�X�@�Y@�X�@�X�@�Yb@�Y�@�Y�@�Y@�X�@�X�@�X@�X@�Y
@�Xa@�X@�Y�@�Y@�X@�W�@�Xg@�X�@�X�@�Y@�Y�@�Y@�X@�W�@�W�@�WR@�W�@�W�@�WR@�V@�V�@�V(@�XK@�W�@�U�@�V�@�W�@�X@�W�@�Y#@�Z/@�XQ@�X@�WV@�XN@�Xg@�X�@�Y�@�Y�@�W�@�Y�@�Y�@�Z�@�Y�@�Y�@�Z@�Y�@�Z�@�Z@�Y�@�Y!@�Y]@�Y�@�X�@�XR@�X�@�X�@�W�@�Y#@�Y@�XQ@�X�@�XN@�Y!@�Y#@�Y@�X�@�X@�Y@�Xb@�X@�X�@�W�@�W�@�W�@�Xf@�Xb@�XP@�X�@�Xa@�W�@�X@�W�@�W�@�Xg@�W<@�V�@�T�@�V�@�W�@�W�@�WB@�Xe@�WR@�Y
@�Y#@�Z0@�Z @�Z�@�[Z@�[�@�[\@�[�@�[�@�[W@�Y]@�Y_@�Y�@�Yb@�X@�V�@�U@�U"@�U"@�R�@�R�@�R�@�U@�Up@�T_@�S�@�Uq@�{[@�z�@�x�@�s�@�t~@�t�@�uP@�t�@�v�@�t�@�y�@�wn@�v�@�y&@�y<@�xm@�x0@�y�@�y�@�zy@�y|@�y�@�yd@�w�@�vt@�q�@�q=@�m�@�m!@�lb@�k�@�l�@�k'@�l@�k�@�oU@�py@�U�@�W�@�\,@�X�@�K�@�H�@�M�@�X�@�X�@�U
@�E:@�4�@�1@�3F@�/�@�)!@� �@�!j@��@��@�!�@�#6@��@��@��@��@��@�@�?@��@�@��@��@�N@�b@�J@��@��@�L@��@�K@�6@�
�@�=@�R@�
�@�M@�R@�#@�
�@�
�@�
�@�
�@�
�@�
Q@�	�@�	i@��@�@�s@�v@��@�v@��@�F@�F@�@�@�@��@��@��@��@�2@�2@�0@�
@��@��@�K@��@�v@��@�x@�y@�$@�R@�@��@��@�n@�@��@��@�o@�p@�v@�1@��@�@��@��@�w@� �@� T@� @� @� �@� T@� C@�<@�;@�j@� �@� T@� �@� �@� �@� R@� ~@� �@� m@� �@� �@� W@� @���@��7@���@���@���@���@���@���@���@���@���@���@���@���@Q��@Q��@Q��@Q�F@Q��@Q��@Q�z@Q��@Q�v@Q�r@Q�s@Q��@Q��@Q��@Q�E@Q��@Q�@Q�@Q�@@Q�@Q�@Q�@@Q��@Q�@Q�>@Q�e@Q��@Q��@Q��@Q�@@Q�@@Q��@Q�n@Q�r@Q�U@Q��@Q@Q}�@Q|H@Q{ @Qzx@Qy�@Qy~@Qy*@Qx@Qw�@Qw]@Qw@Qw2@Qw6@Qv�@Qv@Qu�@Qu�@Qu�@Quf@Qu�@Quc@Qt;@Qq�@Qq�@Qq�@Qr @Qq�@Qp�@Qp�@Qp�@Qr@Qq�@Qo+@QpN@Qpx@Qp�@Qn�@Ql�@Qf&@Qe�@Qe�@Qc�@Q`@@Q]M@QZ@QY�@QW@QV�@QV�@QV�@QVn@QT�@QS�@QR(@QP�@QP�@QOb@QO;@QK�@QK@QIz@QH@QE�@QDe@QA�@Q?(@Q;�@Q6�@Q2b@Q*�@Q%H@Q ^@QM@Q�@Q3@Q�@Q�@Q�@Q	@QM@Q[@Q@P�%@�{[@�z�@�x�@�s�@�t~@�t�@�uP@�t�@�v�@�t�@�y�@�wn@�v�@�y&@�y<@�xm@�x0@�y�@�y�@�zy@�y|@�y�@�yd@�w�@�vt@�q�@�q=@�m�@�m!@�lb@�k�@�l�@�k'@�l@�k�@�oU@�py@�U�@�W�@�\,@�X�@�K�@�H�@�M�@�X�@�X�@�U
@�E:@�4�@�1@�3F@�/�@�)!@� �@�!j@��@��@�!�@�#6@��@��@��@��@��@�@�?@��@�@��@��@�N@�b@�J@��@��@�L@��@�K@�6@�
�@�=@�R@�
�@�M@�R@�#@�
�@�
�@�
�@�
�@�
�@�
Q@�	�@�	i@��@�@�s@�v@��@�v@��@�F@�F@�@�@�@��@��@��@��@�2@�2@�0@�
@��@��@�K@��@�v@��@�x@�y@�$@�R@�@��@��@�n@�@��@��@�o@�p@�v@�1@��@�@��@��@�w@� �@� T@� @� @� �@� T@� C@�<@�;@�j@� �@� T@� �@� �@� �@� R@� ~@� �@� m@� �@� �@� W@� @���@��7@���@���@���@���@���@���@���@���@���@���@���@���@Q��@Q��@Q��@Q�F@Q��@Q��@Q�z@Q��@Q�v@Q�r@Q�s@Q��@Q��@Q��@Q�E@Q��@Q�@Q�@Q�@@Q�@Q�@Q�@@Q��@Q�@Q�>@Q�e@Q��@Q��@Q��@Q�@@Q�@@Q��@Q�n@Q�r@Q�U@Q��@Q@Q}�@Q|H@Q{ @Qzx@Qy�@Qy~@Qy*@Qx@Qw�@Qw]@Qw@Qw2@Qw6@Qv�@Qv@Qu�@Qu�@Qu�@Quf@Qu�@Quc@Qt;@Qq�@Qq�@Qq�@Qr @Qq�@Qp�@Qp�@Qp�@Qr@Qq�@Qo+@QpN@Qpx@Qp�@Qn�@Ql�@Qf&@Qe�@Qe�@Qc�@Q`@@Q]M@QZ@QY�@QW@QV�@QV�@QV�@QVn@QT�@QS�@QR(@QP�@QP�@QOb@QO;@QK�@QK@QIz@QH@QE�@QDe@QA�@Q?(@Q;�@Q6�@Q2b@Q*�@Q%H@Q ^@QM@Q�@Q3@Q�@Q�@Q�@Q	@QM@Q[@Q@P�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        3344344434444444444444444444344444444444444434344444444334444443344444344443344434444444444444344443443443444443433444344444444444443344344444444344334443334443344444434443443344443344333343443434433334343433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9�9P9�9c9�99�999E9v9n99�99O99d9�99>9�9)9�9�9s9�9�9V9�9B9�9�9e9.9H9J9��9��9_9 R9��9�:9��9 U9 Q9�9�9��9�T9�E9�*9�Q9κ9ς9�"9�9ϻ9�9��9�9��9��9�9�`9��9�O9��9�q9�9��9��9��9�89�9��9�79��9��9��9��9�9��9��9�9��9��9��9�}9��9�}9�9��9�R9��9�-9��9��9��9��9��9�Q9�Q9�9�9�9��9�9��9�9�@9�@9�>9�9��9��9�t9��9��9�(9��9��9�o9��9�p9�49�9��9��9�~9�59�9�9�09��9�i9��9��9�D9�19�x9�N9�9�9��9�N9�?9�9�9�C9��9�N9��9�u9�x9�L9�s9��9�d9�w9��9�Q9�9��9��9�+9�=9�-9�c9�=9�c9�/9�w9�c9�a9��9�y8��G8��G8��q8���8���8���8��H8���8��'8��#8��$8��G8��I8��8���8��H8��8��8��8��8��8��8��	8��t8��8��8��Q8��,8��)8��8��8��r8��8��#8��d8���8��8��28��8��8��s8��8��8��L8��H8���8��8��h8��8��8���8���8��B8��B8��8���8��8���8���8���8�ܥ8�ܥ8��8���8��8��8���8��8���8��y8��z8�۟8��8��8��R8�҃8��_8��98��|8��O8�ʴ8���8��c8��78���8���8��8�Ĥ8��W8��.8���8��l8���8��k8��I8��8���8��58���8���8���8���8��8��8���8���8��-8��B8���8���8��?8���8���8��18��l8��d8�~�8�|38�yI8�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBaHB`BB_;B^5B]/B]/B]/B]/B]/B\)B\)B[#BZBYBR�BXB]/B]/BaHBcTBjBl�Bl�Bp�Bt�B�B�B�FB�wB��B��BɺBɺBB��B�hBz�B��B��Bk�Bl�BA�B;dBK�BffB�B�B�bB��B�-B�!B�-B��B��B�B�?B�bB�B� B�=B��B��B�B��B�B�#B�yB,BS�BYB[#BYBP�B'�B��B��B��B��BB��BVB�B�B�BuBJB��B�B�BŢB��B�jB�'B��Bk�B@�BB�B��B�JBp�BcTBk�B�Bw�BXB!�B$�B+BC�BW
BO�BG�B@�B0!B�B�B
=B
�B
��B
��B
��B
�{B
�B
ffB
G�B
7LB
�B
B	�B	�BB	��B	�qB	�XB	�3B	�B	��B	��B	�hB	�B	l�B	`BB	O�B	D�B	9XB	49B	,B	$�B	�B	�B	hB	JB	1B	B	B��B��B��B��B��B�B�yB�fB�fB�`B�ZB�NB�)B�B�B��B��B��B��BɺBƨBŢBÖB��B�qB�dB�LB�?B�3B�!B�B��B��B��B��B��B��B�hB�JB�1B�B�B}�Bw�Bq�Bn�BgmBdZBcTB`BB[#BXBT�BR�BR�BQ�BQ�BP�BO�BN�BL�BJ�BI�BH�BF�BB�B<jB9XB8RB8RB7LB6FB49B1'B2-B/B-B+B)�B(�B+B.B(�B(�B+B+B+B,B0!B33B2-B5?B;dB:^B8RB8RB7LB9XB9XB9XB8RB8RB:^B:^B5?B,B,B.B/B.B0!B1'B2-B7LB8RB9XB:^B;dB?}BA�BB�BB�BC�BE�BI�BK�BM�BN�BN�BO�BQ�BT�BW
BZB[#B\)B_;BbNBcTBe`BgmBm�Bp�Bs�Bu�Bx�B|�B�B�B�B�7B�=B�JB�PB�\B�\B�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�RB�XB�}BBɺB��B��B�
B�#B�/B�ZB�B�B�B�B��B	B	1B	VB	bB	oB	uB	�B	�B	+B	33B	5?B	7LB	:^B	@�B	B�B	D�B	G�B	K�B	L�B	XB	^5B	aHB	e`B	ffB	gmB	gmB	ffB	dZB	bNB	dZB	jB	n�B	u�B	{�B	|�B	}�B	}�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�!B	�B	�3B	�LB	�?B	�-B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�LB	�XB	�XB	�XB	�^B	�dB	�dB	�qB	�}B	B	ĜB	ĜB	ŢB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�BB	�/B	�B	��B	��B	��B	��B	��B	�
B	�5B	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�yB	�fB	�B	�%B
�B
6B
B
B
)*B
2�B
:�B
?.B
F�B
K)B
M�B
RoB
W�B
Z�B
_;B
a�B
i�B
l"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5�B�L?*@?��B�?'�?5��A[��Bp�>�s;>�.
>�G�?;�T>�@�>�-�?�9>��g>�0�?�8?*0@@�J>�!2?8��?��U>ݡ�?[�?�R?n�FB�#>�d�>�O>�҉?�i>�bA?�c@ܲ�>�ݔ>��?�~@( @��J?�?;q�@�B
��?��^B��>��(@Q�>��]>��>Ĳ�?Z?+)�?��B8�B
Z�>�iU?W�A��>�Q>�(�?(�[A�fB��>��?PB�A%2@���?�[�B �_>�A���?
 ?s�OB-�Bap??F�?��)B�>��]>�>��>��?��?t`�A{%�?F_�A��?2/Z>��#?	�j?C��A�L�A�ǔ>�U;?�?�j�A��?+��An*�BH�A6@0?:�=A��|? ^�@���>�}>���?"�>A��?l�B;`BX�?i�?>=�A��BRK>��>�?�t@I>�/�?�A֒>��>�
�>�pa@�$>>�6?4��B=A޶�?�C?��B\A��7?�@�@1N�@���@B`=@��?O��B �?4&?#��A�;B
�At�>��?K�B_�B=�B>�?���?
�?�"FA�9�B;�@���?/4�@�j/?k�@�[�?��0BE�?�f?��?�x}A��?8�A15�B?�BI`A��<>�?<NrA|DcB?�B`�?!q?�6BA�A�9BCBD�@7��A�c�?��@3�BH�A��B�>���?"�4A�\HB�xB_�B?�?`C�BZ(Aa��BG�?i�A�d�BhXB@WB>TB@�BZ2?�lBI:AQ��B@�BBFB@`B<(B?8B@BINB@iB?-BAvBA�BA�BA�BB�BCqBByBCTBD�BF�BC�BBBCgBB�BB�BB\BEBBSBC�BCBBBBBCBCrBC�BC�BBvBA�BBBB�BBfBAFB@<B>�BA>B?B?�B<�BABA�BCIBB�BAFBB�BCoBB$BB"BB�BABB�B>.B>8B?0B;�B=wB>�B?BFBC�BD�BAcBB?BCBCBBBABABA<BA�B@�B@B=�B>KB>SB=5B9-B95B<�B?AB@�BC$BC�BA�BBBB�BB[BBBC?BB�BA�BBQBA�B@{BA'B<XB<�B=PB>�BCB@�BB*BB�BCBB�BC�BB�BC7BCBB�BBBBfBBfBBwB?�B<�B>�B=HB= B;B?�B<0B?�B?�B?KB@�B>�BB�B?�B@`BBBAbBB>BCBA�B?8B@�B@�BB,BA�BD B@uBAvBA�B@�B@�BA>BA�BA�BA�BB�BBBA�BBBBTBBBB\BCBA�BBBA*BB�BD�BC�BC�BBpBC_B@NBA�B?�B?	B=�B@NB@&BA�BAFB?�BB�BABDjBB�B@�B@B>�B?�B?�B>�B>B?9BB�B@NB@�BAB?	BBB?�B>�B?	B?BB>�B@�B=B@CB>�B@�B<�B?CB;B=�BBB<BB?B@BBIB>B=wB>]B?9B=�B@�B>�B>B=mB@�B?�B@DB=!B>oB>6BADBBSBAYB@�B@�BAYB@aBBQB@iBA�BA�BB?BB�BBIBA*B>]BAvBAnBAcBA*B@~BC�BB\BBBCiBA�BA�BB�BC%BBTBD�B?�B?0B>�B?�B@jBA�B?9B?�B@CB?BB�BByBA�BBBC�BASBB�BC�BCBB�BD{BA�BB�BBpBA5BBBC B@�BB�BA�BBBA�BA�B@[B@fBA�B?�B@XBB�BCVBC�BABA0BB�BB�BBRBA�BABA�BAcBBpBA�BB�B@�BA'B@�B@�BAfB?�BAB?�B?�B?kB>�B>�B=B?2B=�B=uB8�B=�B@B>PB<B=�B=TB=DB>dB;|B;�B7�BAGB&�BJBEB<�B:kB;�B?�B4)B%dB8�B8�B<,B:�B5GB3�B99B<sB;�BB�B8�B4�B<�B7�B>2B=bB>�B=yB=qB=�B>>B>SB=�B>:B>wB?KB>qB<�B=�B<�B>B=\B?B><B<�B=�B=fB>(B>3B=�B=�B>B=�B=�B=�B=RB=�B>�B>+B>�B=�B>�B>OB=�B=mB?B>�B>!B=�B>rB>iB>aB>�B>�B>8B>CB=�B=�B>�B=fB=UB<�B=ZB<�B=fB=�B>3B=�B=�B=^B=�B=bB=ZB=zB=�B<�B=YB=�B<�B=�B=MB<�B<�B<�B<�B<�B<tB<B=�B=|B=lB=B=�B;�B<�B<�B<1B<B<�B;�B;�B<�B;B<\B;�B:�B:B:�B<>B;�B<B<B;�B;�B<SB;�B;�B;iB;(B<HB	�]B	�QB	�6B	��B	ԯB	՗B	�B	�B	��B	�B	��B	�BB	�%B	�XB	�xB	թB	ՎB	ՁB	�tB	՗B	֚B	�}B	ՠB	�UB	ՅB	�.B	��B	��B	ՎB	�B	��B	�'B	ӟB	�B	�{B	ԈB	�<B	ֵB	��B	�kB	��B	��B	�#B	�|B	�#B	��B	��B	֤B	�xB	��B	�kB	�nB	�B	��B	��B	ֱB	׈B	��B	�B	�B	�B	ֶB	�!B	�B	ןB	ֿB	ׄB	�B	�B	֗B	�B	ոB	ԵB	�hB	�gB	ԵB	�-B	�B	�OB	��B	ԠB	�B	ԭB	��B	��B	ԒB	�B	�8B	�B	��B	��B	�"B	��B	��B	СB	��B	ѾB	�AB	�IB	��B	��B	�KB	��B	�&B	��B	�sB	�xB	�1B	�1B	�hB	΂B	ήB	ͦB	̬B	�YB	ϰB	��B	��B	ΧB	ϝBb.Ba�Bb�Ba�Ba?Ba{BaBajBa�B`�Ba�BaJB`�Ba1Ba3Ba5B`�BarB`�Bb!Ba+B`�Ba B`�B`�B_mB`qB`)B_�B_�B_SB`
B_WB`"B_B^OBboBa�B^�B`HBd�B^�B]`B^B]�B^mB^%Bb�B]�B\�B]B\�B]�B]B]�B^zB\@B]$B]�B]xB]�B]�B]B]�B\�B]�B]�B\�B]^B\�B]uB]xB]\B\�B]}B]4B\B]B\�B\{B\�B\�B]BB]�B\�B\~B\�B\�B]�B\�B\�B]B\�B\�B\�B\dB\�B\�B\�B\kB\�B]B]B\�B]yB\�B]'B\|B\:B],B]UB]MB]EB]B\�B]rB]B]4B]B\�B\�B\�B]GB]IB\#B\�B\aB\EB\�B\�B\&B\�B\�B\�B]0B\�B\B[�B\fB\JB\B[�B\RB\IB\�B[�B[�B[�B\SB[�B[�B\)B[�B[mB\&B[.B\B\GB[!B[�B[�B[�B[uB[�B[�BZ�BZ�BZ�B[BZ�BZ�BZ�BZ/BZ�BZBZ�BZ�B	�B	�fB	�xB	�B	�XB	�KB	�aB	�B	��B	��B	��B	��B	��B	��B	�"B	�pB	�B	�B	�B	�_B	�RB	�cB	�B	��B	��B	��B	�B	�VB	�<B	��B	��B	�~B	�%B	�_B	�.B	��B	��B	��B	��B	��B	�tB	�B	�B	�ZB	�B	�B	��B	�B	�B	�B	�B	�B	�hB	�NB	�"B	��B	��B	��B	��B	�SB	�B	��B	�=B	�B	�NB	�AB	��B	��B	�B	�B	�zB	�|B	��B	�#B	�B	� B	��B	��B	�MB	��B	�B	�WB	��B	�$B	��B	��B	��B	�kB	�iB	�fB	�XB	�B	�)B	�)B	��B	�mB	�B	��B	��B	�B	�B	�aB	�\B	��B	�B	�MB	��B	�B	�B	�YB	�sB	�bB	�1B	�\B	�yB	�B	�B	�B	�bB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993344344434444444444444444444344444444444444434344444444334444443344444344443344434444444444444344443443443444443433444344444444444443344344444444344334443334443344444434443443344443344333343443434433334343433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999BbDBa?B`<B_1B^+B]%B]#B]%B]%B]#B\B\"B[BZBYBR�BXB]#B]"Ba?BcLBjvBl�Bl~Bp�Bt�B�B��B�?B�mBʵB��BɯBɲBB��B�`Bz�B��B��Bk}Bl�BA~B;YBK�Bf]B�B�B�XB��B�$B�B�#B��B��B��B�4B�YB�B�B�3B�{B��B��B��B�B�B�lB+�BS�BY	B[BYBP�B'�B��B��B��B��B �B��BIB�B�B�BiB<B��B�B�BœB�|B�_B�B��BkyB@uB �B�B��B�=Bp�BcGBkxB��Bw�BXB!�B$�B*�BC�BV�BO�BG�B@xB0B�BuB
1B
�B
��B
�wB
��B
�mB
�B
fXB
G�B
7BB
�B
�B	�~B	�6B	��B	�eB	�NB	�&B	�B	��B	��B	�]B	��B	lB	`5B	O�B	D�B	9KB	4-B	+�B	$�B	�B	yB	\B	>B	#B	B	�B��B��B��B��B��B�B�kB�YB�ZB�TB�NB�@B�B�B��B��B��B��B��BɯBƚBŔBÈB�{B�fB�WB�>B�3B�#B�B�B��B��B��B��B��B��B�\B�;B�!B�B��B}�Bw�Bq�Bn�Bg`BdLBcEB`4B[BXBT�BR�BR�BQ�BQ�BP�BO�BN�BL�BJ�BI�BH�BF�BB�B<[B9JB8FB8DB7=B68B4*B1B2B/B,�B*�B)�B(�B*�B.B(�B(�B*�B*�B*�B+�B0B3#B2B52B;UB:OB8DB8EB7@B9KB9HB9IB8BB8BB:RB:OB50B+�B+�B.B/B.B0B1B2B7>B8BB9HB:NB;TB?nBA}BB�BB�BC�BE�BI�BK�BM�BN�BN�BO�BQ�BT�BV�BZB[B\B_+Bb?BcGBePBg^Bm�Bp�Bs�Bu�Bx�B|�B��B�B�B�(B�.B�:B�BB�MB�KB�RB�YB�cB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�*B�DB�IB�pBBɫB̼B��B��B�B�B�MB�B�B�B�B��B	 �B	B	GB	SB	`B	dB	sB	�B	*�B	3$B	50B	7=B	:OB	@qB	B�B	D�B	G�B	K�B	L�B	XB	^'B	a7B	eOB	fVB	g\B	g]B	fWB	dHB	b?B	dJB	jpB	n�B	u�B	{�B	|�B	}�B	}�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�FB	�RB	�lB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�"B	�>B	�2B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�'B	�?B	�HB	�HB	�HB	�OB	�TB	�SB	�_B	�kB	�B	čB	čB	œB	ŔB	ȥB	ɪB	˶B	˴B	˷B	̽B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�%B	�1B	�1B	� B	�B	��B	��B	��B	��B	��B	��B	�'B	�>B	�LB	�PB	�^B	�hB	�pB	�tB	�uB	�pB	�lG�O�B	�B	�B
�B
%B
B
�B
)B
2�B
:�B
? B
F�B
KB
MvB
R_B
W�B
Z�B
_+B
a�B
i�B
lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5�B�DG�O�G�O�B�G�O�G�O�G�O�Bp�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��G�O�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B8�B
Z�G�O�G�O�G�O�G�O�G�O�G�O�A�fB��G�O�G�O�G�O�G�O�G�O�B �WG�O�G�O�G�O�G�O�B-�BafG�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�L�G�O�G�O�G�O�G�O�A��G�O�G�O�BH�G�O�G�O�A��lG�O�G�O�G�O�G�O�G�O�A��G�O�B;VBX�G�O�G�O�G�O�BRAG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B<�A޶�G�O�G�O�B\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B �G�O�G�O�A�0B
�G�O�G�O�G�O�B_�B=�B>�G�O�G�O�G�O�A�9~B;�G�O�G�O�G�O�G�O�G�O�G�O�BE�G�O�G�O�G�O�A��G�O�G�O�B?�BIZG�O�G�O�G�O�G�O�B?�B`�G�O�G�O�BA�A�&BCBD�G�O�A�c�G�O�G�O�BH�G�O�B��G�O�G�O�A�\?B�mB_�B?�G�O�BZG�O�BG�G�O�A�d�BhPB@QB>HB@�BZ)G�O�BI1G�O�B@�BB;B@VB<B?.B@BIEB@`B?#BAlBA�BA�BA�BB�BCiBBqBCJBD�BF�BC�BBBC]BB�BB�BBOBEBBKBC�BCBB BBBCBCgBC�BC|BBlBA�BBBB�BB\BA=B@2B>�BA4B?B?�B<�BA BA�BC@BB�BA=BB�BCeBBBBBB�BABB�B>&B>.B?#B;�B=mB>�B?BFBC�BD�BAXBB7BCBCBBBABA BA.BA�B@�B@B=�B>AB>HB=)B9 B9*B<�B?7B@�BCBC�BA�BBBB�BBTBA�BC:BB�BA�BBHBA�B@nBAB<LB<�B=HB>�BCB@�BBBB�BCBB�BC�BB�BC,BB�BB�BBBB\BB\BBlB?�B<�B>uB=?B=B;B?�B<(B?�B?�B?DB@�B>�BB�B?�B@VBA�BAXBB4BCBA�B?.B@�B@�BB$BA�BC�B@jBAlBA�B@�B@�BA4BAxBA�BA�BB�BBBA�BBBBKBBBBOBCBA�BBBA BB�BD�BCxBC�BBeBCTB@BBA�B?�B>�B=�B@BB@BA�BA=B?�BB�BABD`BB�B@�B?�B>�B?�B?�B>�B>B?0BB�B@BB@�BA B>�BBB?�B>�B>�B?5B>�B@�B=B@8B>�B@�B<�B?:B;B=�BA�B<BB7B@BB@B>B=oB>SB?0B=�B@xB>�B>B=fB@�B?�B@8B=B>gB>.BA;BBKBALB@�B@�BALB@XBBKB@`BA�BA�BB7BB�BB@BA B>SBAlBAeBAXBA B@sBC�BBOBBBC`BA�BA�BB�BCBBKBD�B?�B?#B>�B?�B@`BA�B?0B?�B@8B?BByBBqBA�BB	BC�BAJBB�BC�BCBB�BDpBA�BB�BBjBA,BBBCB@xBB�BA�BA�BA�BA�B@NB@`BA�B?�B@QBB�BCLBC�BABA"BB�Bb"Ba�Bb�Ba�Ba6BapBaBa^Ba�B`�Ba�Ba?B`�Ba$Ba)Ba+B`�BaiB`�BbBa!B`�B`�B`�B`�B_cB`kB`!B_�B_�B_LB`B_LB`B_B^DBbdBa�B^�B`ABd�B^B]XB]�B]�B^_B^Bb�B]�B\�B\�B\�B]�B]B]�B^pB\7B]B]�B]pB]�B]�B\�B]�B\�B]�B]B\�B]TB\�B]oB]oB]TB\�B]qB]*B\vB\�B\�B\rB\�B\�B]:B]{B\�B\tB\�B\�B]�B\�B\�B\�B\�B\�B\�B\\B\B\{B\{B\bB\�B]	B\�B\�B]oB\�B]B\rB\0B]#B]IB]CB]<B]B\�B]gB] B]*B]B\�B\�B\�B]<B]>B\B\�B\YB\:B\�B\yB\B\�B\�B\�B]%B\�B\B[�B\^B\@B[�B[�B\IB\@B\�B[�B[B[�B\IB[�B[�B\B[B[dB\B[$B\B\=B[B[�B[�B[�B[lB[�B[�BZ�BZ�BZ�B[BZ�BZ�BZ�BZ&BZ�BY�BZ�BZ�B	�pB	�UB	�hB	�B	�IB	�:B	�SB	�B	��B	��B	�B	��B	��B	��B	�B	�_B	�B	�vB	�B	�NB	�BB	�TB	�zB	��B	��B	��B	�tB	�HB	�-B	��B	�B	�mB	�B	�NB	�B	��B	�B	�B	�B	��B	�cB	��B	�B	�KB	�vB	� B	��B	�B	�B	�B	��B	�B	�WB	�=B	�B	��B	��B	�B	��B	�DB	��B	��B	�.B	�B	�?B	�2B	��B	��B	�B	�B	�jB	�lB	�B	�B	�B	�B	��B	�B	�<B	�B	�B	�FB	��B	�B	��B	�B	��B	�[B	�ZB	�WB	�GB	�B	�B	�B	��B	�]B	��B	�B	�B	��B	�	B	�QB	�KB	��B	�B	�>B	��B	��B	�B	�IB	�dB	�QB	�$B	�LB	�iB	�B	�B	�B	�PB	��Bb"Ba�Bb�Ba�Ba6BapBaBa^Ba�B`�Ba�Ba?B`�Ba$Ba)Ba+B`�BaiB`�BbBa!B`�B`�B`�B`�B_cB`kB`!B_�B_�B_LB`B_LB`B_B^DBbdBa�B^�B`ABd�B^B]XB]�B]�B^_B^Bb�B]�B\�B\�B\�B]�B]B]�B^pB\7B]B]�B]pB]�B]�B\�B]�B\�B]�B]B\�B]TB\�B]oB]oB]TB\�B]qB]*B\vB\�B\�B\rB\�B\�B]:B]{B\�B\tB\�B\�B]�B\�B\�B\�B\�B\�B\�B\\B\B\{B\{B\bB\�B]	B\�B\�B]oB\�B]B\rB\0B]#B]IB]CB]<B]B\�B]gB] B]*B]B\�B\�B\�B]<B]>B\B\�B\YB\:B\�B\yB\B\�B\�B\�B]%B\�B\B[�B\^B\@B[�B[�B\IB\@B\�B[�B[B[�B\IB[�B[�B\B[B[dB\B[$B\B\=B[B[�B[�B[�B[lB[�B[�BZ�BZ�BZ�B[BZ�BZ�BZ�BZ&BZ�BY�BZ�BZ�B	�pB	�UB	�hB	�B	�IB	�:B	�SB	�B	��B	��B	�B	��B	��B	��B	�B	�_B	�B	�vB	�B	�NB	�BB	�TB	�zB	��B	��B	��B	�tB	�HB	�-B	��B	�B	�mB	�B	�NB	�B	��B	�B	�B	�B	��B	�cB	��B	�B	�KB	�vB	� B	��B	�B	�B	�B	��B	�B	�WB	�=B	�B	��B	��B	�B	��B	�DB	��B	��B	�.B	�B	�?B	�2B	��B	��B	�B	�B	�jB	�lB	�B	�B	�B	�B	��B	�B	�<B	�B	�B	�FB	��B	�B	��B	�B	��B	�[B	�ZB	�WB	�GB	�B	�B	�B	��B	�]B	��B	�B	�B	��B	�	B	�QB	�KB	��B	�B	�>B	��B	��B	�B	�IB	�dB	�QB	�$B	�LB	�iB	�B	�B	�B	�PB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111119999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999993344344434444444444444444444344444444444444434344444444334444443344444344443344434444444444444344443443443444443433444344444444444443344344444444344334443334443344444434443443344443344333343443434433334343433333343433333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333111122222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311647262020083116472620200831164726202008311647262020083116472620200831164726202008311647262020083116472620200831164726202008311647262020083116472620200831164726AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191816372019021918163720190219181637    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816372019021918163720190219181637  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191816372019021918163720190219181637  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311647262020083116472620200831164726  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                