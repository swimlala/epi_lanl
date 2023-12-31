CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:17:23Z creation      
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
resolution        =���   axis      Z        %P  E   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T  j`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %P  s�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %P  �X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P  Ǩ   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P  �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P $�   CNDC         
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     %P J@   CNDC_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T o�   CNDC_ADJUSTED            
      	   	long_name         Electrical conductivity    standard_name         !sea_water_electrical_conductivity      
_FillValue        G�O�   units         mhos/m     	valid_min                	valid_max         A     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     %P x�   CNDC_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T �4   CNDC_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mhos/m     C_format      %12.5f     FORTRAN_format        F12.5      
resolution        8ѷ     %P ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	T  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %P *    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Op   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   P0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   \0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   h0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � t0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   t�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   t�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   t�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   t�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � u   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                  , u�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   u�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                  0 v    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        v0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        v<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       vH   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0 vTArgo profile    3.1 1.2 19500101000000  20190219181723  20200831164946  5903273 5903273 5903273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 US ARGO PROJECT                                                 STEPHEN RISER                                                   STEPHEN RISER                                                   STEPHEN RISER                                                   PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL               �   �   �AAA AOAOAO  3334                            3334                            3334                            2C  2B  2C  DAD APEX                            APEX                            APEX                            4917                            4917                            4917                            041310                          041310                          041310                          846 846 846 @�xX
=|�@�xX
=|�@�xX
=|�111 @�xXffr�@�xXffr�@�xXffr�@7�~��"�@7�~��"�@7�~��"��cO�E����cO�E����cO�E���111 GPS     GPS     GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                 Near-surface sampling: discrete, unpumped [auxiliary STS]                                                                                                                                                                                                       Secondary sampling: discrete [high frequency 0.1 dbar data, for cross-calibration with the STS]                                                                                                                                                                    �   �   �ADA BDA  DA BDA @,��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�33A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Dy��D��D�8�D��D���D��D�J�D�~fD��3D�	�D�;�D�\DǼ)D��D�K�Dڀ D�ƸD��D�ED�r�D��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O������������������������������������������������������L��    �������������L�;�������������������������������    ���;L�;��������������������L�;L�;�����������    ���;�������    �L�;����������ͽ��;��������L�;L�;����L�;��������L�;L�;��������������������������;��������L��    �L�;�����������    ���������������������������������������������������;L�;������������������;��������L�;��������������������L�;����������������L�;��������L�ͽ��ͽ��;����L�;L�;��������L�;����������;�������������������=��ͽ��;����L�;L�;�������������������    �L�;������������L�;L�;��������L��    ���������L�;L�;������������������;������������������������L�;������������L�;����L�;��������������;L�;����L�;L�;����L�;��������L�;L�ͽ��;������������L�;��������L�;L�;L�;������ͽ��ͽ��;����������������L�;L�ͽ��ͽ��;����L�;L�;����L�;L�;����L�ͽ��;L�;L�;L�ͽ���    �L�;��������L�ͽ���    �L�;L�;L�ͽ��ͽ��ͽ���    �L��            ����        ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���        ����    ����        ����    ���ͽ��ͽ��ͽ��ͽ���    ���ͽ��ͽ��;L�ͽ��ͽ��;L�ͽ��ͽ���            ����            ���ͽ��;L�ͽ��ͽ���    �L��    �L�ͽ���                    ���ͽ���    ���ͽ��ͽ��ͽ��ͽ���                ����=���        ����    �L�ͽ��ͽ���=���    ����            =��ͽ���    ����    ���;L�ͽ��;L�ͽ���                    ���ͽ��ͽ���                ����    ���ͽ��ͽ��ͽ���    =���            ���ͽ���    ���ͽ��ͽ��ͽ���    ���ͽ���                ���ͽ��ͽ��ͽ��;L��    ����        �L�ͽ��ͽ��ͽ��ͽ���    ����            ����    ���ͽ��ͽ���            ���ͽ��ͽ��ͽ��ͽ��ͽ���        ����=���                ����    �L��    ����        ���ͽ���    ���ͽ��ͽ���=��ͽ���    ���ͽ��ͽ���    ���;L�ͽ���    ���;L�ͽ��ͽ��ͽ��;L��    ����    ����    =���    �L�ͽ��;L��    ����    ����                ���;L�;L�ͽ��ͽ��ͽ���    ���ͽ���        ���;L�ͽ��ͽ���    ���ͽ���                ����    =���            ���ͽ��ͽ��ͽ��ͽ��ͽ��ͽ���    ����    �L�ͽ��;L�;L��                =���>���>���>���>���?   ?   ?��?333?333?L��?L��?fff?�  ?���?���?�ff?�ff?�33?�33?���?���?ٙ�?�ff?�33@   @ff@��@��@33@��@   @,��@333@9��@Fff@S33@Y��@`  @l��@s33@�  @�33@���@�  @�33@���@���@�33@���@�  @�33@���@�  @�ff@ə�@�  @�ff@���@�  @�ff@���@�  @�ff@���A��A��AffA	��A��A  A33A��A  A��A��AffA!��A$��A(  A)��A,��A0  A333A4��A8  A;33A<��A@  AC33AFffAI��AK33ANffAQ��AS33AVffAY��A\��A`  Ac33AfffAh  Ak33AnffAq��At��Ax  Ay��A|��A�  A���A�33A���A���A�33A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�33A���A���A�33A���A�ffA�  Ař�A�33A���A�ffA�  A͙�A�33A�  Aљ�A�33A���A�ffA�  Aٙ�A�33Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3DqٚDq�fDq��Dq�3Dr  DrfDr3Dr�Dr&fDr,�Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr��Dr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��DtfDt�Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��@   @,��@333@9��@Fff@S33@Y��@`  @l��@s33@�  @�33@���@�  @�33@���@���@�33@���@�  @�33@���@�  @�ff@ə�@�  @�ff@���@�  @�ff@���@�  @�ff@���A��A��AffA	��A��A  A33A��A  A��A��AffA!��A$��A(  A)��A,��A0  A333A4��A8  A;33A<��A@  AC33AFffAI��AK33ANffAQ��AS33AVffAY��A\��A`  Ac33AfffAh  Ak33AnffAq��At��Ax  Ay��A|��A�  A���A�33A���A���A�33A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�ffA�  A���A�33A���A�ffA�  A���A�33A���A���A�33A���A�ffA�  Ař�A�33A���A�ffA�  A͙�A�33A�  Aљ�A�33A���A�ffA�  Aٙ�A�33Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��Dp� Dp��Dp�3Dp� Dp�fDp�3Dp��DqfDq�Dq3Dq  Dq&fDq33Dq9�DqFfDqL�DqY�Dq` Dql�Dqs3Dq� Dq�fDq��Dq��Dq� Dq��Dq�3Dq� Dq�fDq�3DqٚDq�fDq��Dq�3Dr  DrfDr3Dr�Dr&fDr,�Dr9�Dr@ DrL�DrS3DrY�DrffDrl�Drs3Dr� Dr��Dr�3Dr��Dr�fDr��Dr��Dr� Dr��Dr�3Dr� Dr�fDr��Dr��Ds  Ds�Ds3Ds  Ds&fDs,�Ds9�Ds@ DsL�DsS3Ds` DsffDss3Dsy�Ds� Ds��Ds�3Ds� Ds�fDs�3Ds��Ds�fDs��DsٚDs� Ds�fDs�3Ds��DtfDt�Dt�Dt  Dt,�Dt33Dt9�DtFfDtL�DtY�Dt` Dtl�Dts3Dty�Dt�fDt��Dt��Dt� Dt��Dt�3Dt��Dt�fDt��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@9��@�ff@�ffA33A$��AC33Ac33A���A���A���A���A���A���A�fgA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB̙�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CHL�CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�gDq�Dq��Dr�Dr�gDs�Ds��Dt�Dt��Dy��D�!�D�?\D��zD��\D��D�P�D���D�ٙD�RD�A�D���D�D�
=D�Q�DچfD��D��D�K�D�x�D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����ν��ν��ν��ν��ν��ν��ν��ν��ν��ν��ν��ν��αL��>L�ͽ��ν��ν��αL�ͽ��ν��ν��ν��ν��ν��ν��ν���>L��=��ͱL�ͽ��ν��ν��ν��ν��αL�ͱL�ͽ��ν��ν���>L��=��ͽ��ν���>L�ͱL�ͽ��ν���=���=��ͽ��ν��αL�ͱL�ͽ��αL�ͽ��ν��αL�ͱL�ͽ��ν��ν��ν��ν��ν���=��ͽ��ν��αL��>L�ͱL�ͽ��ν���=���>L�ͽ��ν��ν��ν��ν��ν��ν��ν��ν��ν��ν��ν���=��ͱL�ͽ��ν��ν��ν���=��ͽ��ν��αL�ͽ��ν��ν��ν��ν��αL�ͽ��ν��ν��ν��αL�ͽ��ν��αL��=���=��ͽ��αL�ͱL�ͽ��ν��αL�ͽ��ν���=��ͽ��ν��ν��ν���=���>���=��ͽ��αL�ͱL�ͽ��ν��ν��ν��ν���>L�ͱL�ͽ��ν��ν��αL�ͱL�ͽ��ν��αL��>L�ͽ��ν��αL�ͱL�ͽ��ν��ν��ν���=��ͽ��ν��ν��ν��ν��ν��αL�ͽ��ν��ν��αL�ͽ��αL�ͽ��ν��ν���=��ͱL�ͽ��αL�ͱL�ͽ��αL�ͽ��ν��αL�ͱL��=��ͽ��ν��ν��αL�ͽ��ν��αL�ͱL�ͱL�ͽ���=���=���=��ͽ��ν��ν��ν��αL�ͱL��=���=��ͽ��αL�ͱL�ͽ��αL�ͱL�ͽ��αL��=��ͱL�ͱL�ͱL��=���>L�ͱL�ͽ��ν��αL��=���>L�ͱL�ͱL�ͱL��=���=���=���>L�ͱL��>L��>L��>L��=���>L��>L��=���=���=���=���=���=���=���=���=���>L��>L��=���>L��=���>L��>L��=���>L��=���=���=���=���=���>L��=���=���=��ͱL��=���=��ͱL��=���=���>L��>L��>L��=���>L��>L��>L��=���=��ͱL��=���=���>L�ͱL��>L�ͱL��=���>L��>L��>L��>L��>L��=���=���>L��=���=���=���=���=���>L��>L��>L��>L��=���>���>L��>L��=���>L�ͱL��=���=���>���>L��=���>L��>L��>L��>���=���>L��=���>L��=��ͱL��=��ͱL��=���>L��>L��>L��>L��>L��=���=���=���>L��>L��>L��>L��=���>L��=���=���=���=���>L��>���>L��>L��>L��=���=���>L��=���=���=���=���>L��=���=���>L��>L��>L��>L��=���=���=���=��ͱL��>L��=���>L��>L�ͱL��=���=���=���=���>L��=���>L��>L��>L��=���>L��=���=���=���>L��>L��>L��=���=���=���=���=���=���>L��>L��=���>���>L��>L��>L��>L��=���>L�ͱL��>L��=���>L��>L��=���=���>L��=���=���=���>���=���>L��=���=���=���>L��=��ͱL��=���>L��=��ͱL��=���=���=��ͱL��>L��=���>L��=���>L��>���>L�ͱL��=��ͱL��>L��=���>L��=���>L��>L��>L��>L��=��ͱL�ͱL��=���=���=���>L��=���=���>L��>L��=��ͱL��=���=���>L��=���=���>L��>L��>L��>L��=���>L��>���>L��>L��>L��=���=���=���=���=���=���=���>L��=���>L�ͱL��=��ͱL�ͱL��>L��>L��>L��>L��>���?   ?   ?   ?��?333?333?L��?fff?fff?�  ?�  ?���?���?�fg?�34?�  ?�  ?���?���?�fg?�fg?�34@   @ff@��@33@��@��@   @&fg@,��@9��@@  @Ffg@S33@`  @ffg@l��@y��@�  @�ff@���@�  @�ff@���@�  @�33@���@�  @�ff@���@�  @�ff@���@�  @�ff@���@�33@�ff@���@�33@�ff@���A��A��A  A	��A��A  A33AffA  A33A��A   A!��A$��A(  A+33A,��A0  A333A6ffA8  A;33A>ffA@  AC33AFffAI��AL��ANffAQ��AT��AVffAY��A\��A`  Ac33AfffAi��Ak33AnffAq��At��Ax  A{33A|��A�  A���A�34A���A�fgA�34A���A�fgA�  A���A�34A���A�fgA�  A���A�fgA�  A���A�34A���A�fgA�  A���A�34A�  A���A�34A���A�fgA�  A���A�34A�  A���A�34A���A�fgA�  A���A�34A���A�fgA�34A���A�fgA�  Ař�A�34A���A�fgA�  A͙�A�34A���Aљ�A�34A���A�fgA�  Aٙ�A�34A���Dp��Dp��Dp� Dp��Dp�3Dp� Dp�gDp��DpٚDp� Dp��Dp�3Dq  DqgDq3Dq�Dq  Dq,�Dq33Dq@ DqFgDqS3DqY�DqfgDql�Dqy�Dq� Dq��Dq�3Dq��Dq�gDq��Dq��Dq� Dq��Dq�3Dq� Dq�gDq�3Dq��Dr  Dr�Dr3Dr  Dr&gDr33Dr9�DrFgDrL�DrY�Dr` DrfgDrs3Dry�Dr� Dr��Dr��Dr� Dr�gDr�3Dr��Dr�gDr��DrٚDr� Dr��Dr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` Dsl�Dss3Ds� Ds�gDs��Ds��Ds� Ds��Ds�3Ds� Ds�gDs�3DsٚDs�gDs��Ds�3Dt  DtgDt3Dt�Dt&gDt,�Dt9�Dt@ DtFgDtS3DtY�DtfgDtl�Dty�Dt� Dt�gDt�3Dt��Dt�gDt��Dt��Dt� Dt�gDt�3Dtٚ@,��@9��@@  @Ffg@S33@`  @ffg@l��@y��@�  @�ff@���@�  @�ff@���@�  @�33@���@�  @�ff@���@�  @�ff@���@�  @�ff@���@�33@�ff@���@�33@�ff@���A��A��A  A	��A��A  A33AffA  A33A��A   A!��A$��A(  A+33A,��A0  A333A6ffA8  A;33A>ffA@  AC33AFffAI��AL��ANffAQ��AT��AVffAY��A\��A`  Ac33AfffAi��Ak33AnffAq��At��Ax  A{33A|��A�  A���A�34A���A�fgA�34A���A�fgA�  A���A�34A���A�fgA�  A���A�fgA�  A���A�34A���A�fgA�  A���A�34A�  A���A�34A���A�fgA�  A���A�34A�  A���A�34A���A�fgA�  A���A�34A���A�fgA�34A���A�fgA�  Ař�A�34A���A�fgA�  A͙�A�34A���Aљ�A�34A���A�fgA�  Aٙ�A�34A���Dp��Dp��Dp� Dp��Dp�3Dp� Dp�gDp��DpٚDp� Dp��Dp�3Dq  DqgDq3Dq�Dq  Dq,�Dq33Dq@ DqFgDqS3DqY�DqfgDql�Dqy�Dq� Dq��Dq�3Dq��Dq�gDq��Dq��Dq� Dq��Dq�3Dq� Dq�gDq�3Dq��Dr  Dr�Dr3Dr  Dr&gDr33Dr9�DrFgDrL�DrY�Dr` DrfgDrs3Dry�Dr� Dr��Dr��Dr� Dr�gDr�3Dr��Dr�gDr��DrٚDr� Dr��Dr�3Dr��DsgDs�Ds�Ds  Ds,�Ds33Ds9�DsFgDsL�DsY�Ds` Dsl�Dss3Ds� Ds�gDs��Ds��Ds� Ds��Ds�3Ds� Ds�gDs�3DsٚDs�gDs��Ds�3Dt  DtgDt3Dt�Dt&gDt,�Dt9�Dt@ DtFgDtS3DtY�DtfgDtl�Dty�Dt� Dt�gDt�3Dt��Dt�gDt��Dt��Dt� Dt�gDt�3DtٚG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�G�A�E�A�C�A�G�A�G�A�G�A�G�A�K�A�I�A�I�A�I�A�M�A�G�A�33A�7LA�33A�33A�;dA�;dA�?}A�A�A�=qA�"�A��A��A�1A�A�A�A���A���A���A���A�  A���A���A���A��mA��
A���A��
A��A��HA��TA��A��A��A��A��A��A��A��mA��/A�(�A�
=A���A�=qA�|�A�$�A���A�=qA��+A��A��-A� �A�ffA�  A�v�A���A���A�x�A�G�A��A�;dA�1A���A��HA�l�A���A�$�A�ȴA�bNA��HA�dZA��mA�S�A�bA���A�
=A�ƨA�^5A��A��
A�hsA���A��!A�9XA��A�+A�1'A��mA�;dA�G�A���A�XA��TA���A�
=A��DA�|�A��A��TA�jA�VA�S�A�-A��A��!A�n�A�`BA��^A�jA�/A��TA��DA�M�A|(�Ay�Aw��As\)An�!AkG�Ai��Ah1'Ad�uAax�A^1'A[?}AX�`AX�9AXr�AV��AU��ARĜAQ��AQ��AQC�AO�#AN�9AM��AM"�AMp�AMG�AL��AL  AK��AK+AJ�jAIO�AG��AD��ABz�A@�A@(�A?33A<bA;A:-A8r�A7K�A5��A4VA4A3��A1/A/�wA.�!A.5?A-�#A-XA,�A+S�A*�A*ZA)�^A)%A(�jA(�A%��A%�PA$��A$JA#|�A!�;A!�A�FA��A��AI�A�`A�7AQ�A�A��A��Ar�AM�AbA�FA�uAC�A��A�^A%A9XAdZA�#A(�A"�A
�jA
v�A
$�A	S�A�AJA{A�mA�A�jA �A �@��H@���@��`@���@��R@��w@�;d@�@���@���@�ƨ@�{@�@�dZ@�5?@�?}@�(�@ꟾ@�Ĝ@��@�P@�33@�!@�j@�@��T@�-@�E�@�V@�z�@�
=@��@��@��T@�hs@ԓu@�n�@��@֧�@Չ7@�  @�J@�(�@�Q�@��
@ϕ�@�K�@�;d@�"�@��y@θR@Η�@�M�@͑h@˅@��@��T@ɲ-@Ɂ@ɉ7@�`B@��/@��@�C�@���@�@��H@�ff@�O�@ċD@�(�@�o@¸R@���@��@�I�@���@��@���@�n�@�5?@��-@��/@��@�Z@�1'@��m@�\)@���@��!@�ȴ@�v�@�7L@���@���@��9@��@� �@� �@��@��@�\)@�
=@��H@���@�v�@�-@��^@�p�@�p�@��h@�`B@�I�@�;d@�~�@���@�V@��^@��@��@��j@�I�@�9X@��@��w@���@�dZ@��!@��#@��#@��h@�G�@�Ĝ@�1@��@�E�@���@�7L@�V@��j@�bN@�"�@�@�~�@���@��@���@���@���@�p�@���@�r�@�z�@���@�9X@��m@���@��@��@��D@���@��m@��
@�|�@���@��@�p�@��7@��7@�  @�\)@�"�@�|�@��@�;d@�
=@��@�"�@�+@���@��@�E�@�@��7@��7@��@�hs@�%@��j@� �@��P@�K�@�+@��@���@�n�@��@��h@��@���@���@��@� �@���@��w@���@���@��F@��@��y@�=q@���@��-@��7@�x�@�x�@�X@��@��@�Q�@�  @���@��@�t�@�S�@�33@���@�+@�S�@�C�@��@�;d@�K�@�K�@�C�@�33@��R@��#@��@�x�@�`B@�7L@�V@�Ĝ@��/@��@��@�Ĝ@��u@�(�@��@���@�n�@�E�@�M�@�M�@��@�"�@���@|��@u*0@m��@fQ@_E9@X�@OK�@Gj�@B��@;�P@6�@0PH@,z�@'Z�@ �@�@�e@��@N<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A��yA��A��FA�S�A�A���A�VA�1'A��wA�&�A��A��A��A�1'A�v�A�VA�XA��^A��\A�ffA�"�A�-A�l�A��A�v�A�A��A�{A��A���A�M�A���A���A���A���A��;A�r�A�dZA�JA��A�33A���A�VA�%A��A�ĜA���A� �A�z�A�M�A��A��A��A�XA�{A��A�z�A��A���A�jA���A���A�G�A�(�A�ZA���A�&�A�hsA�A��A��TA�ȴA�$�A�oA�?}A���A�\)A��7A���A�oA��A�-A�=qA�A��`A��/A�jA���A�ffA���A�  A��DA�ȴA���A��mA���A�ffA���A���A��TA�A�z�A���A��A�l�A���A�A�A��#A�hsA�G�A�  A�VA��#A�l�A��+A��
A�;dA��RA�JA�C�A��A��TA��`A���A���A�;dA��A��A��A��A��A���A��-A��TA�/A��A�A�{A�9XA���A�O�A���A��+A�l�A�$�A��FA��A���A�A�A���A�JA�A��HA��+A�?}A��A��9A�JA���A�Q�A�K�A��-A��A�oA���A���A���A�VA���A���A�hsA��;A�%A�(�A��+A�oA�"�A�S�A��A��jA�ĜA���A�$�A��A�`BA�dZA���A��!A��A���A��RA��A�$�A���A��yA�&�A�(�A�&�A�n�A���A��A�5?A�&�A�ȴA�&�A���A���A�"�A�"�A���A���A��A�hsA� �A��A�r�A��A���A�+A� �A�1'A�VA�bA��A�-A�+A��A��A�&�A��A�(�A�+A�&�A�+A�+A�&�A� �A�-A�/A�-A�-A�/A�-A�-A�-A�-A�+A�-A�-A�(�A�+A�/A�-A�/A�/A�-A�/A�1'A�/A�1'A�1'A�-A�/A�/A�/A�/A�-A��yA�/A�-A�+A�1'A�-A�/A�33A�/A�-A�/A�-A�1'A�/A�-A�-A�$�A�1'A�/A�-A�-A�-A�/A�1'A�1'A�/A�/A�/A�/A�/A�-A�33A�33A�33A�33A�/A�-A�/A�/A�+A�-A�7LA�33A�1'A�-A�1'A�1'A�1'A�33A�/A�-A�/A�/A�33A�33A�-A�/A�1'A�1'A�1'A�&�A�(�A�-A�(�A�/A�33A�33A�33A�33A�/A�&�A�"�A�33A�+A�33A�-A�-A�+A�1'A�/A�/A�+A�&�A�/A�33A�1'A�9XA�7LA�"�A�7LA�=qA�9XA�;dA�9XA�7LA�;dA�9XA�=qA�=qA�9XA�5?A�33A�5?A�7LA�;dA�7LA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�?}A�9XA�7LA�;dA�9XA�=qA�=qA�;dA�7LA�?}A�9XA�;dA�9XA�;dA�=qA�;dA�9XA�;dA�;dA�7LA�7LA���A�=qA�=qA�?}A�?}A�=qA�9XA�;dA�=qA�=qA�9XA�;dA�;dA�7LA�5?A�5?A�;dA�=qA�=qA�9XA�&�A�?}A�=qA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�7LA�7LA�+A�5?A�7LA��#A�7LA�33A�7LA�7LA�9XA�;dA�7LA�5?A�/A�(�A�1'A�7LA�7LA�7LA�1'A�9XA�9XA�5?A�5?A�x�A� �A�1'A�/A�33A�33A�/A�/A�5?A�33A�33A�/A�"�A�33A�/A�33A�-A�1'A�/A�1'A�/A�/A�1'A�/A�-A�-A�-A�(�A�&�A�$�A�&�A�+A�(�A�/A�(�A�+A�(�A��A�"�A�&�A�(�A��A�/A�$�A�1'A�/A�5?A�9XA�;dA�=qA�;dA�;dA�9XA�;dA�=qA�?}A�C�A�C�A�G�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�C�A�C�A�E�A�C�A�E�A�E�A�E�A�C�A�E�A�G�A�G�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�C�A�E�A�C�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�C�A�C�A�C�A�C�A�E�A�A�A�C�A�A�A�C�A�E�A�C�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�E�A�I�A�G�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�G�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�E�A�G�A�E�A�G�A�E�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�M�A�K�A�I�A�K�A�I�A�K�A�K�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�I�A�I�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�K�A�K�A�O�A�M�A�Q�A�K�A�I�A�G�A�I�@��u@��D@��@�z�@�z�@�z�@�r�@�r�@�bN@�Z@�Q�@�9X@�9X@�1'@�1'@� �@�  @���@��m@��;@�ƨ@��@���@���@���@��P@��P@��@�|�@�t�@�l�@�dZ@�dZ@�\)@�K�@�+@��@�o@�@��H@�ȴ@���@��!@���@���@��\@��\@��+@��+@�~�@�~�@�~�@�v�@�n�@�n�@�ff@�ff@�ff@�ff@�ff@�ff@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�5?@�5?@�=q@�E�@�V@�V@�V@�V@�^5@�^5@�V@�V@�V@�V@�^5@�ff@�v�@�ff@�V@�V@�=q@�5?@�=q@�5?@�5?@�-@�-@�-@�-@�-@�-@�5?@�-@�J@��@��@�@���@��#@��^@���@��7@�p�@�hsA�G�A�E�A�E�A�E�A�E�A�G�A�C�A�E�A�C�A�C�A�E�A�E�A�G�A�I�A�I�A�I�A�I�A�G�A�E�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�G�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�O�A�Q�A�M�A�G�A�M�A�O�@��u@��D@��D@��@�z�@�z�@�z�@�r�@�j@�bN@�Q�@�A�@�9X@�1'@�1'@�(�@�b@���@��m@��;@���@��@���@���@���@���@��P@��@�|�@�t�@�t�@�l�@�l�@�dZ@�S�@�33@��@�o@�@��y@���@���@��!@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�ff@�ff@�ff@�ff@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�5?@�5?@�=q@�M�@�V@�V@�V@�^5@�^5@�^5@�^5@�V@�V@�^5@�^5@�ff@�v�@�n�@�^5@�V@�M�@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�5?@�-@�-@�-@�{@��@��T@��@��#@��#@���@���@���@��h@�p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999A�E�A�G�A�E�A�C�A�G�A�G�A�G�A�G�A�K�A�I�A�I�A�I�A�M�A�G�A�33A�7LA�33A�33A�;dA�;dA�?}A�A�A�=qA�"�A��A��A�1A�A�A�A���A���A���A���A�  A���A���A���A��mA��
A���A��
A��A��HA��TA��A��A��A��A��A��A��A��mA��/A�(�A�
=A���A�=qA�|�A�$�A���A�=qA��+A��A��-A� �A�ffA�  A�v�A���A���A�x�A�G�A��A�;dA�1A���A��HA�l�A���A�$�A�ȴA�bNA��HA�dZA��mA�S�A�bA���A�
=A�ƨA�^5A��A��
A�hsA���A��!A�9XA��A�+A�1'A��mA�;dA�G�A���A�XA��TA���A�
=A��DA�|�A��A��TA�jA�VA�S�A�-A��A��!A�n�A�`BA��^A�jA�/A��TA��DA�M�A|(�Ay�Aw��As\)An�!AkG�Ai��Ah1'Ad�uAax�A^1'A[?}AX�`AX�9AXr�AV��AU��ARĜAQ��AQ��AQC�AO�#AN�9AM��AM"�AMp�AMG�AL��AL  AK��AK+AJ�jAIO�AG��AD��ABz�A@�A@(�A?33A<bA;A:-A8r�A7K�A5��A4VA4A3��A1/A/�wA.�!A.5?A-�#A-XA,�A+S�A*�A*ZA)�^A)%A(�jA(�A%��A%�PA$��A$JA#|�A!�;A!�A�FA��A��AI�A�`A�7AQ�A�A��A��Ar�AM�AbA�FA�uAC�A��A�^A%A9XAdZA�#A(�A"�A
�jA
v�A
$�A	S�A�AJA{A�mA�A�jA �A �@��H@���@��`@���@��R@��w@�;d@�@���@���@�ƨ@�{@�@�dZ@�5?@�?}@�(�@ꟾ@�Ĝ@��@�P@�33@�!@�j@�@��T@�-@�E�@�V@�z�@�
=@��@��@��T@�hs@ԓu@�n�@��@֧�@Չ7@�  @�J@�(�@�Q�@��
@ϕ�@�K�@�;d@�"�@��y@θR@Η�@�M�@͑h@˅@��@��T@ɲ-@Ɂ@ɉ7@�`B@��/@��@�C�@���@�@��H@�ff@�O�@ċD@�(�@�o@¸R@���@��@�I�@���@��@���@�n�@�5?@��-@��/@��@�Z@�1'@��m@�\)@���@��!@�ȴ@�v�@�7L@���@���@��9@��@� �@� �@��@��@�\)@�
=@��H@���@�v�@�-@��^@�p�@�p�@��h@�`B@�I�@�;d@�~�@���@�V@��^@��@��@��j@�I�@�9X@��@��w@���@�dZ@��!@��#@��#@��h@�G�@�Ĝ@�1@��@�E�@���@�7L@�V@��j@�bN@�"�@�@�~�@���@��@���@���@���@�p�@���@�r�@�z�@���@�9X@��m@���@��@��@��D@���@��m@��
@�|�@���@��@�p�@��7@��7@�  @�\)@�"�@�|�@��@�;d@�
=@��@�"�@�+@���@��@�E�@�@��7@��7@��@�hs@�%@��j@� �@��P@�K�@�+@��@���@�n�@��@��h@��@���@���@��@� �@���@��w@���@���@��F@��@��y@�=q@���@��-@��7@�x�@�x�@�X@��@��@�Q�@�  @���@��@�t�@�S�@�33@���@�+@�S�@�C�@��@�;d@�K�@�K�@�C�@�33@��R@��#@��@�x�@�`B@�7L@�V@�Ĝ@��/@��@��@�Ĝ@��u@�(�@��@���@�n�@�E�@�M�@�M�G�O�@�"�@���@|��@u*0@m��@fQ@_E9@X�@OK�@Gj�@B��@;�P@6�@0PH@,z�@'Z�@ �@�@�e@��@N<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A��yA��A��FA�S�A�A���A�VA�1'A��wA�&�A��A��A��A�1'A�v�A�VA�XA��^A��\A�ffA�"�A�-A�l�A��A�v�A�A��A�{A��A���A�M�A���A���A���A���A��;A�r�A�dZA�JA��A�33A���A�VA�%A��A�ĜA���A� �A�z�A�M�A��A��A��A�XA�{A��A�z�A��A���A�jA���A���A�G�A�(�A�ZA���A�&�A�hsA�A��A��TA�ȴA�$�A�oA�?}A���A�\)A��7A���A�oA��A�-A�=qA�A��`A��/A�jA���A�ffA���A�  A��DA�ȴA���A��mA���A�ffA���A���A��TA�A�z�A���A��A�l�A���A�A�A��#A�hsA�G�A�  A�VA��#A�l�A��+A��
A�;dA��RA�JA�C�A��A��TA��`A���A���A�;dA��A��A��A��A��A���A��-A��TA�/A��A�A�{A�9XA���A�O�A���A��+A�l�A�$�A��FA��A���A�A�A���A�JA�A��HA��+A�?}A��A��9A�JA���A�Q�A�K�A��-A��A�oA���A���A���A�VA���A���A�hsA��;A�%A�(�A��+A�oA�"�A�S�A��A��jA�ĜA���A�$�A��A�`BA�dZA���A��!A��A���A��RA��A�$�A���A��yA�&�A�(�A�&�A�n�A���A��A�5?A�&�A�ȴA�&�A���A���A�"�A�"�A���A���A��A�hsA� �A��A�r�A��A���A�+A� �A�1'A�VA�bA��A�-A�+A��A��A�&�A��A�(�A�+A�&�A�+A�+A�&�A� �A�-A�/A�-A�-A�/A�-A�-A�-A�-A�+A�-A�-A�(�A�+A�/A�-A�/A�/A�-A�/A�1'A�/A�1'A�1'A�-A�/A�/A�/A�/A�-A��yA�/A�-A�+A�1'A�-A�/A�33A�/A�-A�/A�-A�1'A�/A�-A�-A�$�A�1'A�/A�-A�-A�-A�/A�1'A�1'A�/A�/A�/A�/A�/A�-A�33A�33A�33A�33A�/A�-A�/A�/A�+A�-A�7LA�33A�1'A�-A�1'A�1'A�1'A�33A�/A�-A�/A�/A�33A�33A�-A�/A�1'A�1'A�1'A�&�A�(�A�-A�(�A�/A�33A�33A�33A�33A�/A�&�A�"�A�33A�+A�33A�-A�-A�+A�1'A�/A�/A�+A�&�A�/A�33A�1'A�9XA�7LA�"�A�7LA�=qA�9XA�;dA�9XA�7LA�;dA�9XA�=qA�=qA�9XA�5?A�33A�5?A�7LA�;dA�7LA�;dA�;dA�=qA�=qA�;dA�;dA�;dA�?}A�9XA�7LA�;dA�9XA�=qA�=qA�;dA�7LA�?}A�9XA�;dA�9XA�;dA�=qA�;dA�9XA�;dA�;dA�7LA�7LA���A�=qA�=qA�?}A�?}A�=qA�9XA�;dA�=qA�=qA�9XA�;dA�;dA�7LA�5?A�5?A�;dA�=qA�=qA�9XA�&�A�?}A�=qA�;dA�;dA�;dA�;dA�;dA�=qA�;dA�;dA�;dA�7LA�7LA�+A�5?A�7LA��#A�7LA�33A�7LA�7LA�9XA�;dA�7LA�5?A�/A�(�A�1'A�7LA�7LA�7LA�1'A�9XA�9XA�5?A�5?A�x�A� �A�1'A�/A�33A�33A�/A�/A�5?A�33A�33A�/A�"�A�33A�/A�33A�-A�1'A�/A�1'A�/A�/A�1'A�/A�-A�-A�-A�(�A�&�A�$�A�&�A�+A�(�A�/A�(�A�+A�(�A��A�"�A�&�A�(�A��A�/A�$�A�1'A�/A�5?A�9XA�;dA�=qA�;dA�;dA�9XA�;dA�=qA�?}A�C�A�C�A�G�A�E�A�E�A�E�A�G�A�E�A�E�A�E�A�C�A�C�A�E�A�C�A�E�A�E�A�E�A�C�A�E�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�G�A�C�A�E�A�C�A�C�A�E�A�E�A�G�A�I�A�I�A�I�A�I�A�G�A�E�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�G�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�O�A�Q�A�M�A�G�A�M�A�O�@��u@��D@��D@��@�z�@�z�@�z�@�r�@�j@�bN@�Q�@�A�@�9X@�1'@�1'@�(�@�b@���@��m@��;@���@��@���@���@���@���@��P@��@�|�@�t�@�t�@�l�@�l�@�dZ@�S�@�33@��@�o@�@��y@���@���@��!@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�ff@�ff@�ff@�ff@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�5?@�5?@�=q@�M�@�V@�V@�V@�^5@�^5@�^5@�^5@�V@�V@�^5@�^5@�ff@�v�@�n�@�^5@�V@�M�@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�5?@�-@�-@�-@�{@��@��T@��@��#@��#@���@���@���@��h@�p�A�G�A�E�A�E�A�E�A�E�A�G�A�C�A�E�A�C�A�C�A�E�A�E�A�G�A�I�A�I�A�I�A�I�A�G�A�E�A�G�A�E�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�E�A�E�A�C�A�E�A�E�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�E�A�E�A�E�A�E�A�E�A�G�A�G�A�G�A�I�A�G�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�G�A�G�A�G�A�I�A�I�A�G�A�I�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�K�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�O�A�Q�A�M�A�G�A�M�A�O�@��u@��D@��D@��@�z�@�z�@�z�@�r�@�j@�bN@�Q�@�A�@�9X@�1'@�1'@�(�@�b@���@��m@��;@���@��@���@���@���@���@��P@��@�|�@�t�@�t�@�l�@�l�@�dZ@�S�@�33@��@�o@�@��y@���@���@��!@���@���@���@��\@��\@��+@�~�@�~�@�~�@�~�@�v�@�n�@�ff@�ff@�ff@�ff@�ff@�ff@�^5@�V@�V@�M�@�M�@�M�@�M�@�M�@�M�@�E�@�E�@�E�@�=q@�=q@�5?@�5?@�5?@�=q@�M�@�V@�V@�V@�^5@�^5@�^5@�^5@�V@�V@�^5@�^5@�ff@�v�@�n�@�^5@�V@�M�@�=q@�=q@�5?@�5?@�5?@�-@�-@�-@�5?@�-@�-@�-@�{@��@��T@��@��#@��#@���@���@���@��h@�p�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999933333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�=�C-=�FJ?��V=��>M� =�5>8�@Z\=y��=�wp=�in=�]?�x�@�h�@�q�>�m=㢜?Ћ�@�gM=�7=|��=��=�},=���@��+>��@�@�@�la@�m	=�W ?�s�=�A�?��?�D|>0��@��@�J�=�5?>C�@�h
@�g�?|�>�H@��o@�V.> ��>��?��@�W�@��&>�@h�5@�nn>��>5B@�f�=��>D#@�h@�<�=V�L=���=�Wi=��>�@F��@�g�=�QD>P��@�`�@�hI=�2v>\8�@ 7a@�c�@��1>��?=�:=�aR=�%@;g�>d�`@�!W=���>f�>D�?��X@�a?�">6�@�ff>��@Y�>@�d>i�J?�B1@�dE={�=��=���>v�@R�@��\=��|=��U=�NQ>SH,@o��=�'(>:1�@�a|@�^?x�?��T>տ�@��=���>Jw@�l>(b�@���@�d0=�� =�q7=�c>s9@�k<@�lL>w6>��e@�k�>�i�=�0�=ݷk=�vu>��@x@�nY=x��=��X=���?���>#{J=�!�=���@P@�i@�nY>�U@�`�@�h�? h4=��=�f'> k{@�+@�R=n�=�F>%.s=���=�/�?�D�@�y}=��@?��>8��?���@c��@A�M>$�h@\�?�WT@�t?@�^�>cn/@�m	?��V?���@�S�=���>�]y@�s�@�q"@�{�=���@%��>́�@�r�=�w�>�O7@�s.@�s.?Y�p@8f@�u:@�u:@M�
@Xvu=��=�^t>��@�t?@�r@�r�@P��>�W�@�u:@��@//�>���@�NQ>��@�p&@�VC>8�R@�n�@�r�@�u�@�t�@��2?߽'@�r@�vK@�v�@�wG@�u�@�'�@�u:@�r�@�vK@�vK@�v�@�vK@�u�@�s�@�v�@�wG@�wG@�w�@�v�@�v�@�v�@�v�@�wG@�v�@�vK@�v�@�wG@�u�@�v�@�w�@�wG@�w�@�w�@�w�@�x�@�x�@�x�@�x�@�x�@�xW@�x�@�x�@�xW@�w�@�x@�vK@�x@�wG@�vK@�x@�v�@�xW@�x�@�x@�xW@�x@�w�@�v�@�x@�w�@�w�@�wG@�w�@�v�@�w�@�v�@�xW@�x�@�y�@�yS@�x@�w�@�xW@�xW@�x@�x@�y@�x�@�x�@�x�@�x�@�z@�y@�yS@�x@�x�@�yS@�x�@�xW@�yS@�z@�y�@�{@�y�@�x�@�v�@�x�@�z@�zc@�z�@�x�@�x�@�y@�xW@�xW@�u:@�v�@�wG@�x�@�y�@�{t@�z�@�z�@�z@�wG@�u�@�v�@�xW@�z@�{_@�x�@�z�@�{@�{ @�x@�w�@�w�@�w�@�{�@�z@�~=@�{�@�x�@�z�@�}@�}k@�|p@�|�@�}�@�|�@�}@�~(@�}k@�},@�|�@�{�@�z@�{_@�|�@�|p@�|�@�}�@�~(@�}k@�}�@�}@�}k@�},@�}@�}@�}k@�~(@�~|@�9@�|�@�}k@�~|@�~(@�|�@�|�@�|p@�}k@�}@�|p@�|@�~|@�},@�|�@�}�@�z�@�~(@�~|@�~|@�~�@�~(@�}k@�}k@�~(@�~(@�~=@�},@�}k@�{�@�{_@�|@�}k@�}�@�~|@�~|@�}�@�~(@�}�@�}k@�}�@�}k@�}@�~|@�}�@�}@�|�@�|p@�}@�}k@�z�@�{_@�{_@�zc@�{@�{@�{t@�{�@�{�@�{_@�{@�z�@�yS@�zc@�|@�|@�|@�{_@�zc@�|p@�|@�{_@�yS@R	@�w�@�yS@�y�@�y�@�x�@�yS@�yS@�y�@�yS@�y�@�zc@�x�@�yS@�x�@�x�@�x�@�x�@�x�@�xW@�xW@�x@�x@�w�@�wG@�w�@�v�@�u�@�u:@�t�@�u�@�v�@�wG@�wG@�wG@�wG@�xW@�vK@�u�@�xW@�xW@�x�@�yS@�z@�{@�z�@�}@�|�@�~=@�~|@�~(@�}�@�}�@���@���@��4@���@��0@���@���@���@��Z@��0@��0@���@���@��0@���@���@���@���@���@��A@���@��@��A@���@��A@��A@���@���@��k@��V@���@��V@��V@���@���@���@���@��b@���@��#@��b@���@��b@��{@���@���@���@��f@���@���@��'@��'@���@��@���@���@���@��b@���@���@��@��@���@��s@��s@���@���@���@��@��@���@��n@���@��/@���@���@���@���@��s@���@��/@��D@��/@���@��+@���@��@��@���@��<@��<@���@���@���@��<@���@���@���@���@��<@���@���@���@��<@���@��L@���@���@��L@���@��H@��@��@��Y@��n@���@���@��@��n@��n@��@��]@��n@��n@��n@���@���@��D@��n@��n@���@���@��@���@���@�� @��@��@��@��T@��~@��~@���@��e@���@���@��&@���@��~@���@��z@���@���@��7@���@���@���@���@��G@���@��7@���@��7@���@R2v@R1�@R0@R/Z@R.�@R.�@R,�@R+�@R)�@R'�@R%@R#@R"@R!@R!@R\@R�@R�@RZ@R8@RJ@R	@R@R_@R@R�@Rl@R@R�@R *@Q�X@Q��@Q�7@Q��@Q�@Q�@Q��@Q��@Q�@Q�@Q��@Q��@Q��@Qߏ@Q�@@Q��@Q�D@Qܜ@Qۡ@Q�#@Q��@Qڥ@Q�@Q��@Q�Z@Q�@Q؄@Q�0@Q�@Qײ@Q׈@Q�
@Q��@Q��@QՑ@Qջ@Q�=@QՑ@Q�g@Q�@Q��@Q��@Q�k@Q��@Qә@Q�o@Q��@Q�=@Q�^@Q�U@Q�@Q�Q@Q�#@Q�@Q�H@Q��@Q��@Q��@Q�H@Q�n@Q��@Qߏ@Q�e@Q�n@Q�H@Q��@Q؄@Q�0@Qײ@Q�^@Q֌@Q�@Q�b@Q��@Q�9@Q�=@Q��@Q��@QЦ@Q��@Q�d@Qȟ@Q�K@Qł@QË@Q�@Q�g@Q��@Q�x@Q�x@���@���@��I@��^@��I@���@��^@���@���@��
@���@��Z@���@��@��@���@���@��@���@���@���@��@��@��0@��o@���@���@���@��0@��Z@���@���@��@��A@���@��V@��j@��V@���@���@��'@��@���@��#@��#@��#@��b@��b@��w@���@���@���@���@���@��w@���@���@���@���@��@���@���@��4@��r@���@��r@���@��@��Y@���@��H@���@��/@��@��/@��@���@��@��@���@��D@��D@��n@��U@���@���@���@�¤@��P@���@���@��L@��;@��P@�¤@���@�@���@���@��@���@��P@�@���@���@���@���@��a@��a@��v@��"@��L@��a@��v@�Ë@�Ë@��a@�à@��	@��	@�Ĝ@��]@���@���@��	@�à@���@�ı@��X@��m@��@�ŗ@�Ŭ@���@��;@���@�ƨ@��.@�Ŭ@��@Rk�@Rj+@Ri�@Rg�@Rf�@Rff@Rff@Re�@Rd@Rb�@R`�@R]d@R[�@R[@RZq@RX�@RTa@RP�@RM@RJ�@RI@RC�@R@�@R?@R>l@R>�@R=q@R<u@R:�@R9�@R8\@R7�@R7�@R6�@R4D@R.�@R*E@R)t@R&�@R#:@Ry@R�@R�@R�@R�@R'@R@R�@R�@R4@R�@R�@Rb@R�@R�@R�@R�@Ro@R�@RE@Ro@R�@R�@RN@R
�@R$@R
R@R
(@R
(@R	�@R	�@R	W@R	W@R�@R[@R_@R@R_@R�@RN@R�@R�@R�@R�@R4@R^@R�@R�@R�@R4@R0@R@R�@Rr@R�@R+@R�@R�@RA@R�@R@RJ@RJ@Rt@R�@R�@R�@RJ@R�@R@Q��@Q�X@Q��@Q��@Q��@Q�@Q�@Q�=@Q�o@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            44444443444443344434444434333444444334433443344433433443443344444434433444334444444344443443433443444433444434433444344343344443344344444434444444433433444433444444344443443433434434433344434433443333444333343344343343333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@Z]G�O�G�O�G�O�G�O�G�O�@�h�@�q�G�O�G�O�G�O�@�gNG�O�G�O�G�O�G�O�G�O�@��(G�O�@�@�@�lf@�mG�O�G�O�G�O�G�O�G�O�G�O�@��@�J�G�O�G�O�@�h@�g�G�O�G�O�@��p@�V.G�O�G�O�G�O�@�W�@��&G�O�@h�0@�nnG�O�G�O�@�f�G�O�G�O�@�h @�<�G�O�G�O�G�O�G�O�G�O�G�O�@�g�G�O�G�O�@�`�@�hJG�O�G�O�G�O�@�c�@��2G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�!YG�O�G�O�G�O�G�O�@�aG�O�G�O�@�fhG�O�@Y�>@�d
G�O�G�O�@�dDG�O�G�O�G�O�G�O�@R�@��ZG�O�G�O�G�O�G�O�@o��G�O�G�O�@�a�@�^G�O�G�O�G�O�@��G�O�G�O�@�l
G�O�@���@�d.G�O�G�O�G�O�G�O�@�k:@�lNG�O�G�O�@�k�G�O�G�O�G�O�G�O�G�O�G�O�@�nXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�i@�nXG�O�@�`�@�h�G�O�G�O�G�O�G�O�@�+@�RG�O�G�O�G�O�G�O�G�O�G�O�@�y}G�O�G�O�G�O�G�O�@c��G�O�G�O�@\�G�O�@�t>@�^�G�O�@�m
G�O�G�O�@�S�G�O�G�O�@�s�@�q%@�{�G�O�G�O�G�O�@�r�G�O�G�O�@�s/@�s0G�O�G�O�@�u>@�u:@M�
@XvzG�O�G�O�G�O�@�tC@�r @�r�@P��G�O�@�u<@��
G�O�G�O�@�NTG�O�@�p'@�VFG�O�@�n�@�r�@�u�@�t�@��4G�O�@�r@�vJ@�v�@�wJ@�u�@�'�@�u>@�r�@�vJ@�vK@�v�@�vK@�u�@�s�@�v�@�wJ@�wH@�w�@�v�@�v�@�v�@�v�@�wJ@�v�@�vK@�v�@�wJ@�u�@�v�@�w�@�wH@�w�@�w�@�w�@�x�@�x�@�x�@�x�@�x�@�xZ@�x�@�x�@�xU@�w�@�x@�vJ@�x@�wJ@�vK@�x@�v�@�xR@�x�@�x@�xZ@�x@�w�@�v�@�x@�w�@�w�@�wJ@�w�@�v�@�w�@�v�@�xV@�y@�y�@�yX@�x@�w�@�xR@�xU@�x@�x@�y@�x�@�y@�x�@�y@�z@�y@�yS@�w�@�x�@�yW@�x�@�x[@�yV@�z@�y�@�{@�y�@�x�@�v�@�x�@�z@�zf@�z�@�x�@�y@�y@�xZ@�x[@�u>@�v�@�wJ@�y @�y�@�{u@�z�@�z�@�z@�wH@�u�@�v�@�xV@�z@�{\@�x�@�z�@�{@�{ @�x@�w�@�w�@�w�@�{�@�z@�~B@�{�@�x�@�z�@�}@�}m@�|p@�|�@�}�@�|�@�}@�~(@�}n@�}.@�|�@�{�@�z@�{e@�|�@�|r@�|�@�}�@�~)@�}n@�}�@�}@�}n@�}*@�}@�}@�}o@�~)@�~~@�;@�|�@�}f@�~~@�~&@�|�@�|�@�|p@�}f@�}@�|s@�|@�~�@�}*@�|�@�}�@�z�@�~(@�~~@�~}@�~�@�~(@�}n@�}f@�~(@�~(@�~:@�}*@�}f@�{�@�{b@�|@�}m@�}�@�~~@�~~@�}�@�~,@�}�@�}m@�}�@�}m@�}@�~�@�}�@�}@�|�@�|r@�}@�}o@�z�@�{e@�{^@�zf@�{@�{@�{r@�{�@�{�@�{_@�{@�z�@�yS@�zc@�|@�|@�|@�{[@�zd@�|o@�|@�{b@�yQ@R�@�w�@�yV@�y�@�y�@�x�@�yS@�yS@�y�@�yR@�y�@�zd@�x�@�yU@�y@�x�@�x�@�x�@�x�@�x[@�xR@�x@�x@�w�@�wE@�w�@�v�@�u�@�u>@�t�@�u�@�v�@�wG@�wJ@�wG@�wJ@�x[@�vL@�u�@�x\@�xV@�x�@�yS@�z@�{@�z�@�}@�|�@�~;@�~w@�~)@�}�@�}�@���@���@��6@���@��.@���@���@���@��Z@��2@��2@���@���@��1@���@���@���@���@���@��?@���@��@��D@���@���@���@��L@��b@��H@���@��a@���@���@��
@���@��`@���@���@��~@���@���@��@���@���@���@��@��@��3@��p@���@���@���@��2@��Y@���@���@��@��>@���@��V@��i@��S@���@���@��&@��@���@��#@��$@��'@��`@��_@��x@���@���@���@���@���@��x@���@���@���@���@��@���@���@��3@��r@���@��o@���@��@��Z@���@��J@���@��1@��@��.@��	@���@��@��@���@��D@��F@��m@��U@���@���@���@�£@��R@���@���@��M@��;@��R@�£@���@�@���@���@��	@���@��N@�@���@���@���@���@��d@��`@��s@��#@��L@��_@��v@�Ò@�É@��d@�Þ@��
@��
@�Ġ@��^@���@���@��	@�â@���@�İ@��V@��j@��!@�Ŗ@�Ű@���@��;@���@�ƨ@��,@�ū@��@Rk�@Rj0@Ri�@Rg�@Rf�@Rfe@Rfc@Re�@Rd@Rb�@R`�@R]c@R[�@R[@RZp@RX�@RT]@RP�@RM�@RJ�@RI@RC�@R@�@R?@R>h@R>�@R=r@R<r@R:�@R9�@R8]@R7�@R7�@R6�@R4B@R.�@R*C@R)u@R&�@R#;@Rz@R�@R�@R�@R�@R&@R~@R�@R�@R6@R�@R�@Rc@R�@R�@R�@R�@Rm@R�@RE@Rp@R�@R�@RR@R
�@R(@R
P@R
+@R
+@R	�@R
 @R	X@R	Z@R�@RX@Rb@R@R`@R�@RK@R�@R�@R�@R�@R2@R^@R�@R�@R�@R2@R-@R�@R�@Ru@R@R-@R�@R�@R@@R�@R@RJ@RJ@Rr@R�@R�@R�@RJ@R�@R
@Q��@Q�Z@Q��@Q��@Q��@Q�@Q�@Q�=@Q�p@Q�@���@���@��L@��b@��H@���@��a@���@���@��
@���@��`@���@���@��~@���@���@��@���@���@���@��@��@��3@��p@���@���@���@��2@��Y@���@���@��@��>@���@��V@��i@��S@���@���@��&@��@���@��#@��$@��'@��`@��_@��x@���@���@���@���@���@��x@���@���@���@���@��@���@���@��3@��r@���@��o@���@��@��Z@���@��J@���@��1@��@��.@��	@���@��@��@���@��D@��F@��m@��U@���@���@���@�£@��R@���@���@��M@��;@��R@�£@���@�@���@���@��	@���@��N@�@���@���@���@���@��d@��`@��s@��#@��L@��_@��v@�Ò@�É@��d@�Þ@��
@��
@�Ġ@��^@���@���@��	@�â@���@�İ@��V@��j@��!@�Ŗ@�Ű@���@��;@���@�ƨ@��,@�ū@��@Rk�@Rj0@Ri�@Rg�@Rf�@Rfe@Rfc@Re�@Rd@Rb�@R`�@R]c@R[�@R[@RZp@RX�@RT]@RP�@RM�@RJ�@RI@RC�@R@�@R?@R>h@R>�@R=r@R<r@R:�@R9�@R8]@R7�@R7�@R6�@R4B@R.�@R*C@R)u@R&�@R#;@Rz@R�@R�@R�@R�@R&@R~@R�@R�@R6@R�@R�@Rc@R�@R�@R�@R�@Rm@R�@RE@Rp@R�@R�@RR@R
�@R(@R
P@R
+@R
+@R	�@R
 @R	X@R	Z@R�@RX@Rb@R@R`@R�@RK@R�@R�@R�@R�@R2@R^@R�@R�@R�@R2@R-@R�@R�@Ru@R@R-@R�@R�@R@@R�@R@RJ@RJ@Rr@R�@R�@R�@RJ@R�@R
@Q��@Q�Z@Q��@Q��@Q��@Q�@Q�@Q�=@Q�p@Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            44444443444443344434444434333444444334433443344433433443443344444434433444334444444344443443433443444433444434433444344343344443344344444434444444433433444433444444344443443433434434433344434433443333444333343344343343333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9��9��j9��59��J9��29��9��I9��9���9���9��n9��39��9��<9��99��_9��M9���9��9��9��9���9���9��	9��A9���9���9���9��9��,9��S9��9���9���9��N9��9��&9��9��Q9��Q9���9��9��H9��9��9��9���9���9��	9��9��9��A9��f9��9��	9��[9��H9��[9��9��9��A9��T9��9���9��d9���9��;9��u9���9��9���9��)9��9��t9��9��y9��`9��t9��r9���9��9��9���9��9��#9��69��&9���9���9��9��9��x9��|9���9���9��9���9��9���9��99��9��9���9��9��9��9��9���9���9���9��Q9��w9���9���9���9���9���9���9��%9��%9���9��r9���9���9��$9���9���9���9��V9��h9��%9���9���9���9��9���9���9��/9���9���9A �9@�V9@��9@�79@�O9@��9@��9@�B9@��9@�h9@��9@�9@�9@�9@��9@�B9@�R9@��9@�	9@�y9@��9@�M9@�#9@��9@�.9@צ9@�L9@�b9@�9@��9@ѣ9@�,9@��9@��9@��9@Ȝ9@Ĵ9@��9@�B9@�A9@��9@�19@��9@��9@�.9@�l9@��9@�79@�S9@��9@�D9@�D9@��9@��9@��9@��9@��9@�A9@�k9@�9@�D9@��9@��9@�S9@�9@�,9@�f9@�D9@�D9@�9@�9@��9@��9@�9@��9@��9@�h9@��9@��9@�L9@��9@��9@�P9@�59@��9@��9@�B9@�"9@�D9@��9@��9@��9@��9@��9@�J9@��9@��9@��9@�9@�h9@��9@�69@�69@�[9@��9@��9@��9@�69@��9@�P9@��9@�X9@��9@��9@��9@�p9@��9@�Q9@��9@�sG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BJ�BJ�BJ�BJ�BJ�BI�BL�BL�BM�BM�BVB^5B^5BgmBp�Bw�By�Bz�B}�B~�B�B�B�B�B�B�B�B�B�Bo�B<jB=qB]/B\)BZBW
BT�BR�BQ�BP�BK�BK�BK�BM�BO�BO�BO�BP�BQ�BS�BQ�BM�B@�B �BuBVBDB+B��B��B�B�mB�fB�`B�TB�5B�BƨB�jB�?B��B��B�\B�1Bw�BcTBG�B5?B!�B�BhB1BB�B�B�DBo�BR�B7LB+B�BB
�yB
�BB
ǮB
�=B
x�B
n�B
e`B
XB
$�B	��B	�B	�hB	~�B	\)B	6FB	�B	oB	B�yB��B��B�wB�?B�9B�-B�B��B�B�B�!B��BŢB��B��B�B�B��B��B��B��B��B��B�B�fB�B��B��BɺBŢB�dB�XB�^B�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�JB�7B�B~�Bz�Bs�Bo�Bl�BgmBbNB`BB`BB_;B_;B_;B^5B]/B[#B[#B\)BZBXBVBW
BXBS�BS�BO�BM�BK�BJ�BF�BC�B=qB:^B>wBI�BP�BP�BP�BR�BS�BVBXBXBYBYBXBVBW
BW
BS�BQ�BK�BI�BH�BE�BF�BJ�BK�BK�BK�BI�BF�BH�BJ�B_;BjBcTB_;BcTBdZB_;BaHBcTBdZBu�B�B�B�B�B~�B�B�+B�1B�7B�7B�7B�7B�7B�=B�=B�=B�PB�VB�\B�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�XB�qB��BŢBƨB��B��B��B��B��B�B�)B�/B�BB�HB�yB�yB�B�B��B��B��B��B��B��B	  B	B	%B	+B		7B	JB	bB	uB	�B	�B	!�B	)�B	1'B	2-B	0!B	7LB	D�B	D�B	F�B	G�B	I�B	I�B	K�B	L�B	K�B	J�B	I�B	H�B	K�B	N�B	Q�B	S�B	S�B	P�B	M�B	J�B	J�B	J�B	K�B	K�B	I�B	J�B	K�B	K�B	N�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	W
B	YB	[#B	\)B	_;B	ffB	k�B	k�B	iyB	jB	l�B	l�B	l�B	jB	iyB	m�B	m�B	l�B	k�B	k�B	q�B	s�B	u�B	u�B	z�B	}�B	�B	�B	�%B	�%B	�7B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�FB	�RB	�^B	�^B	�^B	�^B	�^B	�XB	�dB	�wB	�wB	�}B	��B	��B	B	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�#B	�/B	�)B	��B	��B
[B
PB
�B
�B
&�B
*�B
2GB
:xB
A B
GB
K�B
M�B
Q�B
U2B
\CB
a�B
f�B
lWB
o�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��?�(A>���?�F�?5?U�ZA�a'>��]>�Q�>�:�?] A*�?B�A�$�?0|�?*�Ai7B5�>���>��r>�>��U?�(B
�?=k�A��B�Be?5T@§�?��@ʷ5@��j?j�HBK�B�?��?F;XA��B�@_$>?N��A��BF?*��?=}[A$�eB�
B�??G�	A��B~?:��?o�RBB?�0?�a�B�A��>�Y:>�]>�R�>�+�?/��A�]�B+�?�9?���B<B#&?$#B?�t2AG��B&B*?C�A9m5>�v�>ߕ�?�	A�}�?�!B)�>�2�?B�w?@��A4%�B9�A'R?pq�B�??}A�0�B�0?���@�)�B!�>���>���>��I?,�nA�&�B�f>�z>��?�I?���AĊ�?2?u�B�B�@��A
��@0�A�
?%�?�k�B�?_�A�XB"�>�z�>�B?B?��PB;B]?���@ ��B#'@&Q7>�v�?�c>�>?;�AlPB�>�A>��k?&\�A��?Z��>�-?% Au��B;B2�?(��A�%By@6S�>��>�X�?R�B�B$	>��E>���?[��?��?&�@œB0?".@N<`?rw�AA8�A���A��Y?WWqA�Y�AQWBBB??���B�@� @�e�B"s? =�@��BwBBBmN>���A��K@k+B-L>��N?�Q�B5B�@�XA��7B�BA���A��u?�i?
�@�bB�B:zB�A��?��0B�B ��A��?�KA���@ >�B�B ��?t/�BAB&tB�B�B:kA**�B�B�BB'B^B��B�B�BB@B)BIB�B1B�B_B�B�BB�BBB_B�B@BB_B�B�B�BVB�B�B�B�B	B�BBaBcB�B)B�B�BB1eBBB_BIBqBB�B8B9BcB9B�BeBBB�B�B�BB6B�B�BlB2B�B�B9B�B�B�BBB
B�BAB�BAB2B�B=ByB�BB.B8B�BSB\B
BUB1B�BB�B-B�B,B�B2BmB�B�B�BiB_B�B�B�B,B,B�B�B�B#B�B�B�B�B�B�B`BBB�B�B!B�B�BXB�B�B�B�B�BfB�B�B�B,B
B�BpB�B_B�BB�B�B�B�B0B�BB,B�BIB�BB'B0BZBcBB|B+B�B�B�BfB|B\B�BB�BIB�B�B/B_B�B�B:B_BMB|B_BhBBRB|B�B�B�B�B�B�BZB B�BB�B�B�B5B�BB5BB�B�B'B�BB3B:�B�B|BGB�B�B�B�B[B�B�BbB�B�B*B�B]BB�BA�R�B�B�B�B1BAByB�B`B�B(B�B�B�B2B8BBaB�B�B�B9BqB�BNB�BB�B�B^B�B�BB�BB'BB�B�B�BB2�ByBABMB�B�B�B*B�BB�B�B@BzBFB@B�BEBBB�B�B�B�B;BFB�B!B�BBBNB�B	BlBB+BB�B�B�B�B-B�B�B�B]B$BbB�BwB�B�BBoBSB�B�B�B�BFB&BlB+B�BLB�B�B�B_B�B]B�B�B�BB!B-BBOB�B�B�B�BB,B�B�BHB@B�BiB�B�B�B�BoB�B�B�B�B�B{B�B�BxB�B�BB�BJB�BB�B�B|B&B]B�BBaB�B.B B�B-B_BkB*B�B�B�BmB�BB�B�B�B�B	BOBBMBcB�BBBjB=B,BBEB\BKB�BB�B^B�BB�B�BxB�B�BWB�B�B�BB<B�B�B�BeB3B	�,B	��B	��B	��B	�jB	�B	��B	� B	��B	�GB	�B	��B	��B	�?B	��B	�B	��B	��B	��B	�B	�UB	�B	�mB	��B	��B	��B	��B	��B	��B	�kB	��B	�0B	�!B	�wB	�BB	��B	�eB	��B	�yB	��B	��B	�iB	��B	�B	�'B	��B	�XB	��B	�B	��B	�{B	�1B	�OB	��B	�nB	�&B	�hB	�B	��B	��B	�oB	�B	�4B	�)B	��B	��B	�zB	��B	�qB	�'B	��B	��B	�zB	� B	��B	��B	��B	��B	�GB	��B	��B	��B	��B	�6B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	�HB	��B	�tB	��B	��B	�	B	��B	�B	��B	��B	�{B	��B	��B	��B	��B	�?B	�!B	�sB	��B	�1B	�iB	��B	��B	�B	�UB	�UB	�WBL(BL�BL{BL�BLaBK�BM,BL�BL�BL�BLOBMBL�BLaBLXBLoBLRBL�BMBL;BMBMBL�BMBLcBL�BLqBLtBL�BL�BL�BMDBL�BL�BL�BL�BL�BMHBL�BL�BMBL�BL�BMBL%BLBLGBMBM	BLtBLwBLIBM0BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BLFBM�BL�BLkBL�BL�BL�BLBL�BMABMBMBLBL�BL�BL�BLcBL�BL�BL�BL�BMABMBLBMBMsBM	BMBM^BLIBLTBL�BL�BL\BL�BLBL�BM7BL�BL�BL;BL�BL�BL�BLnBL]BM0BL�BL�BL�BL�BL%BL�BL�BL�BLXBLFBL�BMMBL�BL�BL�BLTBLkBLdBL�BL�BL�BL�BL5BL`BL�BL�BL�BM�BK�BM.B	��B	ڳB	�,B	��B	�:B	��B	ٷB	�?B	�B	��B	ڇB	�5B	��B	ځB	��B	ٞB	٤B	��B	��B	سB	ٓB	�B	ّB	�tB	��B	�.B	�B	�ZB	�IB	�uB	�HB	��B	ؗB	��B	�:B	�<B	�5B	ٞB	�|B	�:B	��B	��B	��B	�BB	�B	�_B	��B	�SB	ٝB	�B	ٯB	٢B	�KB	�:B	لB	��B	ىB	��B	�B	ٱB	��B	�=B	ڈB	�B	��B	��B	�5B	�	B	��B	��B	ڹB	�1B	�B	ڽB	�UB	ڒB	�HB	�kB	�CB	��B	ڢB	ڧB	�B	گB	��B	��B	�DB	�'B	�,B	�kB	�B	��B	��B	��B	�B	�tB	��B	�_B	��B	�RB	��B	�6B	�9B	�JB	�OB	�oB	�FB	��B	�gB	��B	�8B	ܨB	�B	�}B	�QB	�lB	ۓB	�*B	��B	�QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444443444443344434444434333444444334433443344433433443443344444434433444334444444344443443433443444433444434433444344343344443344344444434444444433433444433444444344443443433434434433344434433443333444333343344343343333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BJ�BJ�BJ�BJ�BJ�BI�BL�BL�BM�BM�BU�B^#B^ Bg\Bp�Bw�By�Bz�B}�B~�B��B��B��B��B��B��B��B��B��Bo�B<[B=aB]!B\BZ	BV�BT�BR�BQ�BP�BK�BK�BK�BM�BO�BO�BO�BP�BQ�BS�BQ�BM�B@nB �BdBDB4BB��B��B�~B�YB�UB�KB�CB�"B��BƔB�WB�+B��B�|B�HB�Bw�BcABG�B5-B!�B�BUBB �B��B��B�0Bo�BR�B77B*�B�B �B
�gB
�0B
ǜB
�,B
x�B
n�B
eMB
W�B
$�B	��B	��B	�SB	~�B	\B	61B	�B	YB	�B�fB��BʫB�bB�)B�%B�B��B��B��B��B�B�nBŌBͽB��B�B�qB��B��B��B��B��B��B�zB�PB��B;BʬBɥBŋB�OB�BB�FB�/B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�jB�]B�PB�4B� B��B~�Bz�Bs�Bo�BlsBgUBb6B`)B`,B_%B_#B_!B^B]B[B[
B\BZBW�BU�BV�BW�BS�BS�BO�BM�BK�BJ�BF�BCB=WB:DB>_BI�BP�BP�BP�BR�BS�BU�BW�BW�BY BX�BW�BU�BV�BV�BS�BQ�BK�BI�BH�BE�BF�BJ�BK�BK�BK�BI�BF�BH�BJ�B_%BjdBc:B_#Bc:Bd?B_"Ba/Bc:BdABu�B��B��B��B��B~�B��B�B�B�B�B�B�B�B�$B�#B�%B�8B�<B�EB�GB�OB�XB�aB�lB�xB�B��B��B��B��B��B��B��B��B��B��B�B�,B�>B�XB�nBņBƏB˯B��B��B��B��B�B�B�B�'B�.B�_B�`B�~B�B��B��B��B��B��B��B��B	B	
B	B		B	0B	GB	ZB	xB	�B	!�B	)�B	1B	2B	0B	73B	D�B	D�B	F�B	G�B	I�B	I�B	K�B	L�B	K�B	J�B	I�B	H�B	K�B	N�B	Q�B	S�B	S�B	P�B	M�B	J�B	J�B	J�B	K�B	K�B	I�B	J�B	K�B	K�B	N�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	V�B	X�B	[B	\B	_$B	fMB	kkB	kjB	i_B	jeB	lqB	lpB	lqB	jdB	i^B	mzB	mxB	loB	kkB	kjB	q�B	s�B	u�B	u�B	z�B	}�B	��B	��B	�B	�B	�B	�DB	�ZB	�bB	�fB	�sB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�-B	�7B	�EB	�EB	�EB	�EB	�BB	�>B	�JB	�\B	�]B	�cB	�iB	�pB	�tB	�{B	ȚB	ˬB	ˬB	͸B	οB	οB	οB	οB	��B	οB	ͺB	ͶB	��B	��B	��B	��B	��B	��B	�B	� B	�	B	�B	�B	��B	�B	�B	�B	�B	�G�O�B	ݴB	�fB
AB
7B
�B
�B
&�B
*}B
2-B
:]B
A	B
F�B
K�B
M�B
Q�B
UB
\*B
a�B
f�B
l>B
o�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�aG�O�G�O�G�O�G�O�G�O�B�A�$sG�O�G�O�G�O�B5�G�O�G�O�G�O�G�O�G�O�B
�G�O�A���B�BTG�O�G�O�G�O�G�O�G�O�G�O�BK�B�G�O�G�O�A��kB�G�O�G�O�A��B5G�O�G�O�G�O�B��B�-G�O�A��BnG�O�G�O�B0G�O�G�O�B�A��G�O�G�O�G�O�G�O�G�O�G�O�B+xG�O�G�O�B,B#G�O�G�O�G�O�BBG�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�G�O�G�O�G�O�G�O�B9�G�O�G�O�BqG�O�A�0xB�!G�O�G�O�B!�G�O�G�O�G�O�G�O�A�&�B�RG�O�G�O�G�O�G�O�AĊ�G�O�G�O�B�BG�O�G�O�G�O�A��G�O�G�O�B�G�O�A�;B"wG�O�G�O�G�O�G�O�B)BKG�O�G�O�B#G�O�G�O�G�O�G�O�G�O�G�O�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B2�G�O�A�BfG�O�G�O�G�O�G�O�B�B#�G�O�G�O�G�O�G�O�G�O�G�O�B0G�O�G�O�G�O�G�O�A���G�O�G�O�A�YxG�O�BBB*G�O�B�G�O�G�O�B"bG�O�G�O�BhB1Bm<G�O�G�O�G�O�B-=G�O�G�O�B$B�G�O�G�O�B�B�A���A��aG�O�G�O�G�O�B�B:iB�A��
G�O�BvB ��G�O�G�O�A���G�O�BpB ��G�O�B2B&dB�B�B:\G�O�B�B�B�BBOBӿB�BzB�B.BB7B�BB�BOBvB�B�B�B�B�BOB�B.B�BOB�B�B�BDB�B�B�B�B�B�B BOBRB�BBB�BB1SB1BOB7B`B�BtB$B'BRB'B�BUB1B�B�B�B�B!B�B�BXB B�B�B'B�BtBB1B�B�B1BB1B B�B+BfB�B�BB$B�BBBKB�BHB!B�B�B�BB�BB�B BYB�B�B�BXBOB�B�B�BBByBvBBB�B�B�B�B�B�BOB1B�BpBB�BrBIB�B�B�B�B�BUB�B�BtBB�B�B`B�BNByB�BtB�B�B�BB�B�BBB5B�B�BBBJBRB�BiBB|B�B�BUBiBLB~BBxB5B�ByB.�BNB�B�B*BNB=BiBNBUBB>BiB�B�B�BtB�B�BJB B�B�BtB�BtB#BxB�B#B�B�B�BBmB�B!B:�B�BkB5B}B�BxB�BJBxB�BPB�B�BB�BJB�B�B�A�R�B�B�B�B!B1BfBpBPB�BBsB�B�B B$B�BOB�B�BtB'B`B�B9B�B�B�B�BMBB�B�BB�BB�B�B�B�B�B2�BfB.B>B�B�BBB�B�B�BtB-BjB5B.BvB3B�B�B�B�B�B�B(B6B�BB�B�B�B:B�B�B[BBLBL�BLkBLxBLNBK�BMBL�BL�BL�BL;BMBL�BLOBLFBL]BLABLuBM	BL+BL�BM BL�BL�BLSBL�BLaBLaBL�BL�BL�BM4BLzBL�BL�BL�BL�BM4BL�BL�BL�BL�BL�BL�BLBLBL4BL�BL�BLaBLfBL6BMBL�BL�BL�BL�BL�BL�BL�BL|BL�BL�BL4BMvBL�BL[BL�BL�BL�BLnBL�BM2BL�BMBLBL�BL�BL�BLOBL�BL�BL�BL�BM4BM BL
BL�BMcBL�BL�BMMBL4BLFBL�BL�BLKBLsBL
BL�BM$BL�BL�BL-BL�BL�BL�BL_BLKBMBL�BL�BL�BL�BLBL�BL�BL�BLFBL4BL�BM;BL�BL�BL�BLDBLWBLSBL�BL�BL�BL�BL%BLOBL�BL~BL�BM�BK�BMB	��B	ڜB	�B	��B	�B	٩B	ٛB	�$B	��B	��B	�mB	�B	��B	�hB	��B	نB	وB	��B	ٯB	؛B	�vB	��B	�uB	�YB	��B	�B	�B	�?B	�-B	�ZB	�-B	��B	�zB	��B	� B	�B	�B	لB	�bB	� B	��B	٧B	ٲB	�)B	��B	�EB	��B	�8B	مB	��B	ٕB	هB	�0B	�!B	�jB	٦B	�nB	��B	��B	ٖB	٨B	�$B	�pB	��B	ھB	��B	�B	��B	��B	٨B	ڟB	�B	��B	ڣB	�8B	�zB	�/B	�PB	�(B	��B	ڇB	ڍB	��B	ږB	۪B	۾B	�)B	�B	�B	�NB	��B	��B	��B	ܸB	��B	�YB	��B	�DB	ܽB	�7B	��B	�B	�B	�.B	�1B	�TB	�+B	��B	�KB	ܯB	�B	ܐB	��B	�bB	�7B	�SB	�xB	�B	��B	�8BLBL�BLkBLxBLNBK�BMBL�BL�BL�BL;BMBL�BLOBLFBL]BLABLuBM	BL+BL�BM BL�BL�BLSBL�BLaBLaBL�BL�BL�BM4BLzBL�BL�BL�BL�BM4BL�BL�BL�BL�BL�BL�BLBLBL4BL�BL�BLaBLfBL6BMBL�BL�BL�BL�BL�BL�BL�BL|BL�BL�BL4BMvBL�BL[BL�BL�BL�BLnBL�BM2BL�BMBLBL�BL�BL�BLOBL�BL�BL�BL�BM4BM BL
BL�BMcBL�BL�BMMBL4BLFBL�BL�BLKBLsBL
BL�BM$BL�BL�BL-BL�BL�BL�BL_BLKBMBL�BL�BL�BL�BLBL�BL�BL�BLFBL4BL�BM;BL�BL�BL�BLDBLWBLSBL�BL�BL�BL�BL%BLOBL�BL~BL�BM�BK�BMB	��B	ڜB	�B	��B	�B	٩B	ٛB	�$B	��B	��B	�mB	�B	��B	�hB	��B	نB	وB	��B	ٯB	؛B	�vB	��B	�uB	�YB	��B	�B	�B	�?B	�-B	�ZB	�-B	��B	�zB	��B	� B	�B	�B	لB	�bB	� B	��B	٧B	ٲB	�)B	��B	�EB	��B	�8B	مB	��B	ٕB	هB	�0B	�!B	�jB	٦B	�nB	��B	��B	ٖB	٨B	�$B	�pB	��B	ھB	��B	�B	��B	��B	٨B	ڟB	�B	��B	ڣB	�8B	�zB	�/B	�PB	�(B	��B	ڇB	ڍB	��B	ږB	۪B	۾B	�)B	�B	�B	�NB	��B	��B	��B	ܸB	��B	�YB	��B	�DB	ܽB	�7B	��B	�B	�B	�.B	�1B	�TB	�+B	��B	�KB	ܯB	�B	ܐB	��B	�bB	�7B	�SB	�xB	�B	��B	�8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999944444443444443344434444434333444444334433443344433433443443344444434433444334444444344443443433443444433444434433444344343344443344344444434444444433433444433444444344443443433434434433344434433443333444333343344343343333343333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333311222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222299999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES            TEMP            CNDC            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CNDC_ADJUSTED = (sw_c3515 * sw_cndr(PSAL_ADJUSTED, TEMP, PRES_ADJUSTED)) / 10                                                                                                                                                                                   PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   none                                                                                                                                                                                                                                                            No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      Unpumped STS data; quality control flags assigned using automated protocols outlined in Argo Quality Control Manual. Additional quality control and/or inspection recommended prior to use                                                                      High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       High-frequecy sampling; data contain repeated/non-monotonic pressure readings for the purposes of cross-calibration with STS data; additional data processing is recommended prior to use                                                                       202008311649462020083116494620200831164946202008311649462020083116494620200831164946202008311649462020083116494620200831164946202008311649462020083116494620200831164946AO  AO  AO  ARCAARCAARCAADJPADJPADJP                                                                                                                                                                                                            201902191817232019021918172320190219181723    IP  IP  IP                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817232019021918172320190219181723  QCP$QCP$QCP$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�5F03E           103E            703E            AO  AO  AO  ARGQARGQARGQQCPLQCPLQCPL                                                                                                                                                                                                            201902191817232019021918172320190219181723  QCF$QCF$QCF$                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�0               0               0               UW  UW  UW  ARSQARSQARSQUWQCUWQCUWQC            WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               WOD & nearby Argo as visual check                               202008311649462020083116494620200831164946  IP  IP  IP                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                