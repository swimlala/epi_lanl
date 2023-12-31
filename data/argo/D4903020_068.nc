CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-11T00:38:37Z creation; 2021-03-26T17:01:02Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � aP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � i8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210311003837  20210326170213  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               D   DAA  AOAO7836_008777_068                 7836_008777_068                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�dx��>W@�dx��>W11  @�dy=�c@�dy=�c@<*��"��@<*��"���d�L�I'��d�L�I'�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@�G�@�G�@��R@�  A   A�RA\)A+�A@  A`��A�Q�A�  A��A��A�  A�  A�\)A�B (�B  B�
B  B�
B(  B0  B8  B@  BG�BO�
BX  B`  BhQ�Bp(�Bx(�B�{B��B�  B�  B��B�  B�{B�(�B�(�B�=qB�(�B�  B�  B�{B��B�  B�{B��B��
B��
B�{B�{B��B��B��
B�  B�{B�{B�{B�{B�=qB�  B��C
=C��C��C  C
  C  C  C  C  C��C��C��C  C  C
=C {C"  C#�C%�HC'��C*  C,
=C.  C0  C2
=C4  C6{C8  C:
=C<  C>
=C@{CB
=CD  CE��CH
=CJ
=CL  CM��CO�CQ��CS��CV  CX
=CZ  C\  C]�HC_��Cb  Cd  Cf
=Ch
=Cj  Cl
=Cn  Cp  Cr  Ct{Cv
=Cw��Cz
=C|  C~
=C�  C�
=C�C�C���C���C���C���C�C�C�C�  C�  C�  C���C�  C���C�  C���C�  C�  C�  C�C�
=C�
=C�  C�  C���C���C�  C�C�  C�  C�  C�  C��C�  C�  C�C�C�
=C�  C�C���C���C�C�C�  C�
=C�  C���C���C�C�C�  C�C�C�  C�  C�  C�C�C�C�  C�C���C���C�C�C���C���C���C���C�  C�C�  C�  C�  C���C���C���C���C�C�  C���C���C�  C�
=C�C�C���C���C���C�  C�  C�  C�  C���C�C�
=C�C�  C�  C���C�C�C�  C�  C���C�  C�C�  C�
=C�  C�  C�
=C�
=C�C�  C���C���C���C���C�  C���C���C���C�D   D � D  D� D  D}qD��D� D�D� D�qD}qD�qD}qD��D}qD  D� D	�D	}qD	�qD
}qD  D�D�D��D  D}qD�qD}qD�qD}qD  D��D�D��D�D��D  D}qD�qDz�D  D��D�D�D�D��DD}qD�qD��D�D� D�D��D  D}qD��D}qD  D� D�qD}qD   D � D �qD!� D"  D"� D#D#��D$D$� D%  D%��D&  D&� D'  D'}qD(  D(}qD(�qD)� D*  D*��D+�D+��D,�D,��D-�D-� D.  D.}qD.�qD/��D0D0��D1�D1��D2�D2� D2�qD3}qD3�qD4� D5  D5� D6�D6� D7  D7}qD8  D8�D9�D9� D:  D:��D;�D;��D<  D<z�D<�qD=}qD>  D>� D?  D?}qD@  D@��D@�qDA��DB�DB}qDB��DC� DD�DD}qDD�qDEz�DE�qDF� DG�DG��DH  DH� DI  DI}qDJ�DJ� DJ�qDK}qDK�qDL� DM  DM}qDN  DN� DO  DO� DO�qDP� DQ�DQ� DR�DR��DS�DS� DT  DT}qDT�qDU}qDV  DV� DV�qDW� DX�DX� DY  DY}qDY�qDZ� DZ��D[z�D\  D\}qD\�qD]� D^  D^��D_  D_}qD`  D`� Da  Da}qDb  Db��Dc  Dc}qDc�qDd��De�De��Df�Df� Dg  Dg}qDh  Dh��DiDi��Di�qDj}qDk�Dk�DlDl�DmDm��Dn  Dn��Do  Doz�Do�qDp��Dq  Dq� Dr  Dr}qDr�qDs}qDs�qDt}qDt�qDu}qDv  Dv� Dw�Dw��Dx  Dxz�Dx�qDy� Dz  Dz� D{�D{� D{�qD|� D}�D}��D~  D~� D~�qD}qD�qD�>�D�}qD��qD���D�>�D�� D��HD���D�@ D���D��HD�  D�@ D�� D���D���D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�HD�@ D�~�D���D���D�@ D��HD�� D�HD�@ D�~�D�� D���D�=qD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D��HD�HD�@ D�~�D��qD��qD�=qD�~�D�� D���D�AHD���D�� D���D�>�D�~�D�� D�  D�AHD�~�D���D�HD�@ D�� D�� D��qD�>�D�~�D�� D�  D�>�D��HD��HD�  D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D�D���D�=qD�~�D��HD�  D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�=qD�}qD�� D�  D�>�D�� D�� D��qD�@ D�� D��qD�HD�AHD�� D�� D���D�>�D�~�D���D�  D�>�D�~�D���D���D�AHD��HD�� D��qD�@ D���D�� D�  D�B�D�� D���D���D�@ D�� D�� D�  D�AHD�~�D��qD���D�@ D�~�D��HD��D�@ D�� D�D�HD�=qD�}qD���D��qD�=qD�� D���D���D�>�D�~�D��HD��D�@ D�� D�� D���D�@ D�~�D�� D��D�B�D�� D�� D���D�>�D�~�D��qD�  D�@ D�}qD��qD�  D�@ D�� D�� D�  D�@ D�~�D��HD�  D�@ D��HD�D�HD�@ D�� D���D�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD���D��qD�=qD��HD�D�  D�>�D�~�D���D�  D�@ D�}qD��qD���D�>�D�� D���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D D��HD��D�B�DÃ�D��HD�  D�@ DāHD��HD�  D�>�Dŀ D�� D���D�>�D�~�D�� D�  D�@ D�~�DǾ�D���D�>�DȁHD��HD�HD�B�DɁHD�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�HD�@ D́HD�� D�  D�@ D̀ D��HD�  D�@ D΀ D��HD�HD�@ Dπ DϾ�D�  D�AHDЀ D�D��D�@ Dр DѾ�D���D�@ DҀ D��HD���D�=qDӀ D�� D�HD�AHD�~�D�� D�  D�@ DՀ DսqD��qD�@ DցHD��HD�HD�AHD�~�D�� D�  D�@ D�~�D�� D�HD�AHDفHD��HD�  D�AHDځHD��HD�HD�@ Dۀ D۾�D�  D�B�D܁HD�� D���D�AHD݁HDݾ�D�  D�@ D�~�D޽qD��qD�>�D߀ D�� D�HD�@ D�}qDྸD�HD�>�D�}qD�� D���D�@ D�HD��HD���D�>�D�HD㾸D���D�@ D�~�D�� D��D�AHD�~�D徸D�  D�AHD�~�D澸D���D�@ D� D�� D�  D�AHD�HD��HD�  D�>�D� D��HD���D�>�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�HD�B�D�HD��HD�  D�AHDD��HD���D�>�D�HD��HD��D�AHD�~�D�� D��D�AHD� D�D��qD�>�D�~�D�D�HD�AHD�HD�� D�  D�@ D�HD�D�  D�@ D��HD���D���D�>�D��HD�D�HD�@ D��HD��HD�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�(�?��?L��?��?���?��?�@�@�R@.{@=p�@L��@\(�@k�@}p�@�ff@�{@�
=@��\@���@�33@�(�@��@���@�@�p�@��@���@�
=@��RA�A��A{A�A
=A(�A   A$z�A(��A-p�A1G�A5A:�HA>�RAC33AHQ�AL��AQG�AUAZ�HA_\)Ac�
AhQ�Al(�Ap  Atz�AxQ�A~{A���A�33A��A�\)A���A��
A�{A�  A�=qA�(�A�ffA���A��HA��A��A��A��
A��RA���A�33A�p�A��A�=qA�(�A��RA���A��HA���A�
=A�G�A�33A�AǮAə�A�(�A�{AУ�Aҏ\A���AָRA���A��HA��A�\)AᙚA��
A�ffA��A�33A��A�A�=qA���A�\)A�G�A�33A�p�A��B ��B{B33BQ�Bp�BffB\)Bz�B	B
=B(�Bp�BffB�B��B�B33Bz�B��B�HB�
B��B{B
=B(�BG�BffB�B ��B!B#
=B$Q�B%p�B&�\B'�B(��B)�B*�HB,  B,��B-�B/
=B0z�B1B3
=B4(�B5p�B6=qB733B8Q�B9p�B:�HB<  B=G�B>�\B?�B@z�BAp�BB�HBDQ�BEG�BF{BG33BHz�BIBK33BLQ�BMp�BNffBO33BPz�BQBS33BTz�BUp�BVffBW\)BX��BY�B[\)B\��B]B^�RB_�B`��BaBc33Bd��Bep�Bf=qBg�Bh��Bj=qBk
=Bl  Bm�Bn�\Bo�
Bp��Bq��Br�HBtQ�Bu��Bv�\Bw\)Bxz�By�B{33B|  B}�B~�\B�B�Q�B���B��B�(�B���B��B��B�ffB�
=B���B�(�B��\B��B��
B��\B�33B��
B�Q�B���B�G�B��
B���B�G�B���B�ffB���B�p�B�=qB��\B�G�B��B�=qB���B��B�  B���B�p�B�B��\B�
=B��B�Q�B��RB���B��B���B�G�B�B��\B��HB�B�Q�B��HB�B�{B��RB���B�  B���B�p�B�B���B��B��
B�z�B���B�B�{B���B��B�{B��HB�G�B�  B���B�33B��B��\B���B�B�ffB��HB�B�(�B��HB��B��B��HB�G�B��B���B�
=B��B�=qB���B���B�=qB��HB��B�(�B���B�\)B�  B�z�B�\)B��B��\B��HB�B�=qB��HB�p�B�{B���B�33B�{B�z�B�33B��B�Q�B�
=B��
B�=qB��HBˮB�(�Ḅ�BͅB�  B�z�B�\)B��
B�ffB�G�B�B�=qB�
=Bә�B�(�B���B�G�B�{BָRB��B�  B�z�B���B�B�ffB���BۮB�=qBܣ�B�p�B�{Bޏ\B�33B�  B�ffB���B�B�Q�B���B�B�=qB��B�G�B�  B��B�
=B�B�z�B���B�p�B�(�B��HB�G�B��B��B�G�B��B�=qB�
=B�B�(�B�RB�B�{B�\B�33B�  B���B��B���B�ffB��B���B�(�B���B��B�  B���B��B��B�z�B�33B�{B�z�B�
=B�C =qC z�C C
=Cp�CC
=CG�C��C  CQ�C�\C�
C�Cz�C�
C33Cz�CC
=CffC��C{CQ�C��C  C\)C�C��C	=qC	z�C	�HC
=qC
z�C
C  CffC�RC{CQ�C��C�CG�C�C�HC(�C�\C�C=qC�CC(�Cz�C�RC{Cz�CC  C\)C�RC{CQ�C��C�HCG�C�C��C33C�C��C(�C�\C�CG�C�C�
C(�C�C�HCG�C��C�HC(�C�\C��CQ�C��C�C33C��C��CQ�C��C�CQ�C�RC��C=qC�C 
=C Q�C ��C ��C!\)C!�RC"
=C"Q�C"��C#  C#\)C#�C#��C$=qC$�C%
=C%\)C%��C%�C&G�C&��C'  C'\)C'��C'�C(33C(��C)  C)G�C)�C)��C*G�C*��C*�HC+=qC+��C,  C,G�C,�\C,��C-\)C-�C-��C.G�C.�RC/  C/Q�C/��C0  C0ffC0�C0��C1Q�C1C2
=C2Q�C2�RC3{C3\)C3��C4
=C4ffC4C5(�C5�C5�HC6(�C6z�C6��C7�C7p�C7��C8(�C8�\C8�HC9=qC9��C9��C:=qC:�\C:�C;=qC;�\C;�C<=qC<��C<��C=Q�C=�C>
=C>ffC>C?(�C?�C?�
C@(�C@z�C@�
CA33CA�CA�
CB(�CB�\CB�CCG�CC��CD  CD\)CDCE�CEffCE�RCF  CFQ�CF�RCG(�CGffCG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ?u@   @B�\@�G�@�G�@��R@�  A   A�RA\)A+�A@  A`��A�Q�A�  A��A��A�  A�  A�\)A�B (�B  B�
B  B�
B(  B0  B8  B@  BG�BO�
BX  B`  BhQ�Bp(�Bx(�B�{B��B�  B�  B��B�  B�{B�(�B�(�B�=qB�(�B�  B�  B�{B��B�  B�{B��B��
B��
B�{B�{B��B��B��
B�  B�{B�{B�{B�{B�=qB�  B��C
=C��C��C  C
  C  C  C  C  C��C��C��C  C  C
=C {C"  C#�C%�HC'��C*  C,
=C.  C0  C2
=C4  C6{C8  C:
=C<  C>
=C@{CB
=CD  CE��CH
=CJ
=CL  CM��CO�CQ��CS��CV  CX
=CZ  C\  C]�HC_��Cb  Cd  Cf
=Ch
=Cj  Cl
=Cn  Cp  Cr  Ct{Cv
=Cw��Cz
=C|  C~
=C�  C�
=C�C�C���C���C���C���C�C�C�C�  C�  C�  C���C�  C���C�  C���C�  C�  C�  C�C�
=C�
=C�  C�  C���C���C�  C�C�  C�  C�  C�  C��C�  C�  C�C�C�
=C�  C�C���C���C�C�C�  C�
=C�  C���C���C�C�C�  C�C�C�  C�  C�  C�C�C�C�  C�C���C���C�C�C���C���C���C���C�  C�C�  C�  C�  C���C���C���C���C�C�  C���C���C�  C�
=C�C�C���C���C���C�  C�  C�  C�  C���C�C�
=C�C�  C�  C���C�C�C�  C�  C���C�  C�C�  C�
=C�  C�  C�
=C�
=C�C�  C���C���C���C���C�  C���C���C���C�D   D � D  D� D  D}qD��D� D�D� D�qD}qD�qD}qD��D}qD  D� D	�D	}qD	�qD
}qD  D�D�D��D  D}qD�qD}qD�qD}qD  D��D�D��D�D��D  D}qD�qDz�D  D��D�D�D�D��DD}qD�qD��D�D� D�D��D  D}qD��D}qD  D� D�qD}qD   D � D �qD!� D"  D"� D#D#��D$D$� D%  D%��D&  D&� D'  D'}qD(  D(}qD(�qD)� D*  D*��D+�D+��D,�D,��D-�D-� D.  D.}qD.�qD/��D0D0��D1�D1��D2�D2� D2�qD3}qD3�qD4� D5  D5� D6�D6� D7  D7}qD8  D8�D9�D9� D:  D:��D;�D;��D<  D<z�D<�qD=}qD>  D>� D?  D?}qD@  D@��D@�qDA��DB�DB}qDB��DC� DD�DD}qDD�qDEz�DE�qDF� DG�DG��DH  DH� DI  DI}qDJ�DJ� DJ�qDK}qDK�qDL� DM  DM}qDN  DN� DO  DO� DO�qDP� DQ�DQ� DR�DR��DS�DS� DT  DT}qDT�qDU}qDV  DV� DV�qDW� DX�DX� DY  DY}qDY�qDZ� DZ��D[z�D\  D\}qD\�qD]� D^  D^��D_  D_}qD`  D`� Da  Da}qDb  Db��Dc  Dc}qDc�qDd��De�De��Df�Df� Dg  Dg}qDh  Dh��DiDi��Di�qDj}qDk�Dk�DlDl�DmDm��Dn  Dn��Do  Doz�Do�qDp��Dq  Dq� Dr  Dr}qDr�qDs}qDs�qDt}qDt�qDu}qDv  Dv� Dw�Dw��Dx  Dxz�Dx�qDy� Dz  Dz� D{�D{� D{�qD|� D}�D}��D~  D~� D~�qD}qD�qD�>�D�}qD��qD���D�>�D�� D��HD���D�@ D���D��HD�  D�@ D�� D���D���D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�HD�@ D�~�D���D���D�@ D��HD�� D�HD�@ D�~�D�� D���D�=qD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D��HD�HD�@ D�~�D��qD��qD�=qD�~�D�� D���D�AHD���D�� D���D�>�D�~�D�� D�  D�AHD�~�D���D�HD�@ D�� D�� D��qD�>�D�~�D�� D�  D�>�D��HD��HD�  D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D�� D�D���D�=qD�~�D��HD�  D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D���D���D�=qD�}qD�� D�  D�>�D�� D�� D��qD�@ D�� D��qD�HD�AHD�� D�� D���D�>�D�~�D���D�  D�>�D�~�D���D���D�AHD��HD�� D��qD�@ D���D�� D�  D�B�D�� D���D���D�@ D�� D�� D�  D�AHD�~�D��qD���D�@ D�~�D��HD��D�@ D�� D�D�HD�=qD�}qD���D��qD�=qD�� D���D���D�>�D�~�D��HD��D�@ D�� D�� D���D�@ D�~�D�� D��D�B�D�� D�� D���D�>�D�~�D��qD�  D�@ D�}qD��qD�  D�@ D�� D�� D�  D�@ D�~�D��HD�  D�@ D��HD�D�HD�@ D�� D���D�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD���D��qD�=qD��HD�D�  D�>�D�~�D���D�  D�@ D�}qD��qD���D�>�D�� D���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�>�D�~�D���D��qD�>�D D��HD��D�B�DÃ�D��HD�  D�@ DāHD��HD�  D�>�Dŀ D�� D���D�>�D�~�D�� D�  D�@ D�~�DǾ�D���D�>�DȁHD��HD�HD�B�DɁHD�� D�  D�@ Dʀ D�� D���D�@ Dˀ D�� D�HD�@ D́HD�� D�  D�@ D̀ D��HD�  D�@ D΀ D��HD�HD�@ Dπ DϾ�D�  D�AHDЀ D�D��D�@ Dр DѾ�D���D�@ DҀ D��HD���D�=qDӀ D�� D�HD�AHD�~�D�� D�  D�@ DՀ DսqD��qD�@ DցHD��HD�HD�AHD�~�D�� D�  D�@ D�~�D�� D�HD�AHDفHD��HD�  D�AHDځHD��HD�HD�@ Dۀ D۾�D�  D�B�D܁HD�� D���D�AHD݁HDݾ�D�  D�@ D�~�D޽qD��qD�>�D߀ D�� D�HD�@ D�}qDྸD�HD�>�D�}qD�� D���D�@ D�HD��HD���D�>�D�HD㾸D���D�@ D�~�D�� D��D�AHD�~�D徸D�  D�AHD�~�D澸D���D�@ D� D�� D�  D�AHD�HD��HD�  D�>�D� D��HD���D�>�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�HD�B�D�HD��HD�  D�AHDD��HD���D�>�D�HD��HD��D�AHD�~�D�� D��D�AHD� D�D��qD�>�D�~�D�D�HD�AHD�HD�� D�  D�@ D�HD�D�  D�@ D��HD���D���D�>�D��HD�D�HD�@ D��HD��HD�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD�� D���D�  G�O�?��?L��?��?���?��?�@�@�R@.{@=p�@L��@\(�@k�@}p�@�ff@�{@�
=@��\@���@�33@�(�@��@���@�@�p�@��@���@�
=@��RA�A��A{A�A
=A(�A   A$z�A(��A-p�A1G�A5A:�HA>�RAC33AHQ�AL��AQG�AUAZ�HA_\)Ac�
AhQ�Al(�Ap  Atz�AxQ�A~{A���A�33A��A�\)A���A��
A�{A�  A�=qA�(�A�ffA���A��HA��A��A��A��
A��RA���A�33A�p�A��A�=qA�(�A��RA���A��HA���A�
=A�G�A�33A�AǮAə�A�(�A�{AУ�Aҏ\A���AָRA���A��HA��A�\)AᙚA��
A�ffA��A�33A��A�A�=qA���A�\)A�G�A�33A�p�A��B ��B{B33BQ�Bp�BffB\)Bz�B	B
=B(�Bp�BffB�B��B�B33Bz�B��B�HB�
B��B{B
=B(�BG�BffB�B ��B!B#
=B$Q�B%p�B&�\B'�B(��B)�B*�HB,  B,��B-�B/
=B0z�B1B3
=B4(�B5p�B6=qB733B8Q�B9p�B:�HB<  B=G�B>�\B?�B@z�BAp�BB�HBDQ�BEG�BF{BG33BHz�BIBK33BLQ�BMp�BNffBO33BPz�BQBS33BTz�BUp�BVffBW\)BX��BY�B[\)B\��B]B^�RB_�B`��BaBc33Bd��Bep�Bf=qBg�Bh��Bj=qBk
=Bl  Bm�Bn�\Bo�
Bp��Bq��Br�HBtQ�Bu��Bv�\Bw\)Bxz�By�B{33B|  B}�B~�\B�B�Q�B���B��B�(�B���B��B��B�ffB�
=B���B�(�B��\B��B��
B��\B�33B��
B�Q�B���B�G�B��
B���B�G�B���B�ffB���B�p�B�=qB��\B�G�B��B�=qB���B��B�  B���B�p�B�B��\B�
=B��B�Q�B��RB���B��B���B�G�B�B��\B��HB�B�Q�B��HB�B�{B��RB���B�  B���B�p�B�B���B��B��
B�z�B���B�B�{B���B��B�{B��HB�G�B�  B���B�33B��B��\B���B�B�ffB��HB�B�(�B��HB��B��B��HB�G�B��B���B�
=B��B�=qB���B���B�=qB��HB��B�(�B���B�\)B�  B�z�B�\)B��B��\B��HB�B�=qB��HB�p�B�{B���B�33B�{B�z�B�33B��B�Q�B�
=B��
B�=qB��HBˮB�(�Ḅ�BͅB�  B�z�B�\)B��
B�ffB�G�B�B�=qB�
=Bә�B�(�B���B�G�B�{BָRB��B�  B�z�B���B�B�ffB���BۮB�=qBܣ�B�p�B�{Bޏ\B�33B�  B�ffB���B�B�Q�B���B�B�=qB��B�G�B�  B��B�
=B�B�z�B���B�p�B�(�B��HB�G�B��B��B�G�B��B�=qB�
=B�B�(�B�RB�B�{B�\B�33B�  B���B��B���B�ffB��B���B�(�B���B��B�  B���B��B��B�z�B�33B�{B�z�B�
=B�C =qC z�C C
=Cp�CC
=CG�C��C  CQ�C�\C�
C�Cz�C�
C33Cz�CC
=CffC��C{CQ�C��C  C\)C�C��C	=qC	z�C	�HC
=qC
z�C
C  CffC�RC{CQ�C��C�CG�C�C�HC(�C�\C�C=qC�CC(�Cz�C�RC{Cz�CC  C\)C�RC{CQ�C��C�HCG�C�C��C33C�C��C(�C�\C�CG�C�C�
C(�C�C�HCG�C��C�HC(�C�\C��CQ�C��C�C33C��C��CQ�C��C�CQ�C�RC��C=qC�C 
=C Q�C ��C ��C!\)C!�RC"
=C"Q�C"��C#  C#\)C#�C#��C$=qC$�C%
=C%\)C%��C%�C&G�C&��C'  C'\)C'��C'�C(33C(��C)  C)G�C)�C)��C*G�C*��C*�HC+=qC+��C,  C,G�C,�\C,��C-\)C-�C-��C.G�C.�RC/  C/Q�C/��C0  C0ffC0�C0��C1Q�C1C2
=C2Q�C2�RC3{C3\)C3��C4
=C4ffC4C5(�C5�C5�HC6(�C6z�C6��C7�C7p�C7��C8(�C8�\C8�HC9=qC9��C9��C:=qC:�\C:�C;=qC;�\C;�C<=qC<��C<��C=Q�C=�C>
=C>ffC>C?(�C?�C?�
C@(�C@z�C@�
CA33CA�CA�
CB(�CB�\CB�CCG�CC��CD  CD\)CDCE�CEffCE�RCF  CFQ�CF�RCG(�CGffCG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A�VA�Q�A�XA�XA�G�A�M�A�G�A�33A�(�A�"�A� �A��A��A��A�oA�JA�%A�A�  A�  A���A��`A��
A��RA���A�~�A�hsA�\)A�1'A��HA��A�z�A�E�A�{A�A��#A�VA���A�ƨA�  A���A�-A��mA�I�A� �A��A���A�O�A��\A�ZA�A���A�K�A�%A���A��wA���A��A�r�A�&�A���A��A�z�A�9XA�A��RA��TA�ƨA�/A��A�I�A�dZA��A��9A��A�A�I�A�z�A��FA���A��`A�l�A��A���A���A�ffA���A��wA�&�A�=qA�ffA�K�A��\A
=A}�A{;dAz�Az�Az�uAyAw�;Aw�Av�HAv�+AvJAu�At�`Ar�+Aq��Ap �Am`BAk��AkVAiC�Ag��Ag`BAg33Ag�Af��Af�yAfr�AeƨAeO�Ac�
AcC�Ab��Aa
=A`bNA`$�A_�wA_�FA_�A_x�A_
=A^�+A^=qA^9XA]VA[K�AZ�AYAX��AV�AV=qAT�AT9XAS�^AR��AP��APM�AOC�AM��AMS�AM
=AL�AK|�AJ�jAJQ�AIx�AIoAH�RAH9XAGC�AF��AE�-ADbNAC|�AC+AB��ABM�AAoA@M�A>��A>A<ȴA<JA;��A;�7A:��A:�\A:9XA9�A8(�A6jA5l�A4~�A3�wA3?}A2�yA2r�A1"�A0JA/�A/7LA.��A.5?A-`BA-�A,��A,jA,bNA,^5A+�
A*VA)�A(�A(�A(I�A&�jA%ƨA$��A#hsA"��A!x�A 1'A�A��A�+AbAbNA�
A�`AE�A�
A��Ap�AS�A33AA�/A��A~�A��A��At�A�\AVAƨAx�A?}A��A�-A�RA��A~�A��A
�HA
  A	�AffA�A��A��A`BA�AA
=Av�A�A�`Ar�AA v�@�|�@�7L@�@���@��@���@��y@�ƨ@�X@�P@�M�@���@���@�&�@�z�@�(�@��@�@�9@�=q@�t�@��@�X@�%@�Ĝ@��;@�K�@�~�@�J@��@݁@�V@�1'@�@��#@١�@���@� �@��y@��@�X@��/@�b@�dZ@�E�@с@�V@Ϯ@�v�@��@�x�@���@�  @�o@�E�@ȴ9@���@¸R@��@�Q�@���@�@�x�@���@�Z@�  @�l�@�n�@���@�&�@�(�@��@�@��T@��`@��P@�
=@�@�hs@��m@�J@��@��@�M�@���@�X@���@��u@�b@�ƨ@��@�;d@���@��@��`@�bN@��F@���@���@���@��j@���@��
@��y@�5?@���@�?}@���@��`@��j@���@���@��R@�J@��-@�/@���@���@�bN@�1'@� �@�  @�ƨ@���@��P@��@�"�@���@��@��h@���@��j@�j@�(�@��w@�;d@��\@�5?@�J@���@���@���@�A�@�b@���@��@��@���@�$�@��#@��h@�%@�Z@�1@��;@��@��P@�;d@��!@�~�@�^5@�M�@�@���@�`B@���@�A�@��
@�C�@��\@�$�@�&�@�b@�|�@�K�@�"�@��@��H@��\@�V@�E�@�J@���@��^@���@��7@��7@�`B@�7L@��@�V@��u@�Q�@�1'@��@�1@�w@;d@}�T@}`B@}?}@}�@|�@|�@|�/@|�/@|��@|�j@|�j@|z�@|Z@|(�@|1@{�m@{��@{t�@z��@z^5@z-@zJ@y��@yhs@yG�@xQ�@w\)@w;d@w�@w
=@v��@v��@v$�@u��@u�-@u�h@u�@u?}@uV@t�j@t9X@s�m@sS�@r�H@r��@r��@r�!@r��@r=q@q��@q�7@qx�@q�@p�u@pb@o�P@o\)@n�@n��@n�+@m�@l��@lj@l1@k�
@k��@kdZ@k"�@ko@j�@j��@j~�@i�7@hĜ@h��@h�@hbN@g\)@f��@f��@f�y@fȴ@f��@fV@e��@d�/@dI�@c�F@c�@cC�@b��@b~�@a�@a��@ahs@aG�@a�@`Ĝ@`A�@_�@_�w@_\)@^��@^V@^$�@]�@]`B@\�/@\��@\�D@[��@[@Z~�@Z�@Y��@Yhs@YG�@Y&�@XQ�@Xb@X  @W�@W��@W
=@V{@U�-@U�@U�@UV@UV@T�D@T(�@S�@SC�@So@R�H@R��@R��@R�@Q�#@Q��@Q��@Q��@QX@Q&�@PĜ@PQ�@PA�@P1'@Pb@O�@O�;@O�;@O�w@O��@O|�@O�@N��@M��@M/@L�@Lz�@K�m@K�
@Kƨ@Kƨ@K��@KdZ@KS�@KC�@K33@K"�@K"�@K"�@K"�@K@J��@J��@JJ@I��@I�@HĜ@Hr�@Hb@G��@G|�@G;d@F��@F�y@F�y@Fȴ@Fv�@E�T@E�-@E��@EO�@D�@D��@D��@D�D@DI�@C��@Ct�@B��@B^5@A��@A�^@AG�@A%@@�9@@r�@@bN@@ �@?�@?�P@?
=@>��@>�+@>v�@>ff@>E�@>5?@>{@=�@=�-@<z�@;S�@:��@:~�@:M�@:J@9��@9X@9�@8��@8�9@8�u@8�@8Q�@8 �@7�w@7|�@7�@6��@6�@6��@6v�@6V@6{@5��@5��@5�@5?}@5�@4�@4�@4j@4I�@49X@4�@3�F@3t�@3dZ@333@3@2�!@2�@1�@1�^@1��@1x�@1X@1G�@0��@0Q�@0  @/|�@/;d@.��@.�@.��@.V@-�T@-��@-��@-�@-`B@-O�@,��@,�@,�@,��@+�
@+S�@+@*�H@*��@*�!@*~�@*=q@*J@)��@)�7@)x�@)X@)X@)%@(Ĝ@(�u@(Q�@(A�@(1'@(b@'�@'��@'�w@'��@'+@&�y@&�R@&��@&v�@&ff@&E�@&$�@&{@%�@%��@%/@%V@$�@$Z@$(�@#��@#��@#�@#S�@"�@"�@"�H@"��@"�!@"�\@"~�@"^5@"J@!�#@!��@!G�@!�@ �`@ r�@ bN@ Q�@  �@  �@ b@�;@�@l�@K�@�@��@��@V@$�@�T@�-@p�@/@V@�/@�j@��@Z@1@��@�m@�
@ƨ@��@t�@dZ@S�@C�@33@�@��@��@^5@�^@��@x�@G�@�@%@��@��@��@r�@ �@b@  @  @�w@|�@K�@+@�@��@�R@�+@$�@{@{@�@�-@`B@/@�@��@�/@Z@��@��@��@�
@��@o@�H@��@��@�!@�\@~�@M�@J@J@��@�#@x�@��@�9@��@�u@r�@Q�@1'@ �@�@��@�w@��@l�@��@V@V@E�@{@��@�@/@��@�j@�@j@9X@(�@(�@�@1@��@�
@��@t�@dZ@S�@C�@
�@
�!@
��@
��@
�\@
n�@
^5@
M�@
�@	��@	�#@	X@	G�@	&�@	�@�`@�@A�@A�@�;@�@��@��@�P@|�@|�@l�@l�@K�@�y@��@�+@v�@ff@ff@V@5?@{@�T@�h@p�@?}@?}@?}@/@��@��@��@�j@�D@z�@Z@�@�@��@ƨ@�F@��@��@t�@dZ@S�@33@o@@�H@��@�\@~�@^5@=qA�hsA�jA�l�A�`BA�VA�XA�S�A�S�A�VA�S�A�Q�A�S�A�S�A�ZA�XA�\)A�XA�VA�XA�K�A�A�A�O�A�G�A�I�A�O�A�Q�A�O�A�O�A�E�A�=qA�1'A�1'A�-A�-A�+A�&�A�$�A�$�A�"�A� �A� �A�"�A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A��A��A��A��A��A��A��A��A��A�VA�JA�JA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�1A�%A�%A�1A�%A�1A�%A�A�A�A�A�A�A�  A�  A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A�  A���A�  A�  A���A��yA��mA��mA��`A��TA��HA��TA��HA��;A��/A��/A��#A��#A��#A�ȴA���A��wA��jA��RA��jA��A��!A��A��A���A���A���A��A��A��A��A��A��A�|�A�|�A�z�A�r�A�n�A�l�A�l�A�jA�dZA�bNA�`BA�`BA�\)A�\)A�\)A�ZA�\)A�S�A�O�A�G�A�;dA�33A�oA�bA���A���A��A��`A��#A���A�ƨA��wA��^A��FA��-A��A��A���A��hA��DA�~�A�z�A�z�A�r�A�ffA�bNA�^5A�Q�A�C�A�7LA�5?A�33A��A��A�{A��A��A�bA�JA�1A�JA�
=A�1A�A���A���A���A���A���A��;A���A���A��^A��A�|�A�p�A�^5A�G�A�7LA�-A�"�A��A�JA�A��A��TA���A��PA�&�A��A��
A��A�r�A�ZA�M�A�9XA�oA���A��`A�ȴA���A��wA��A���A���A��hA��A�p�A�\)A�O�A�9XA�$�A�oA�A���A���A���A�ZA��jA�ZA��A���A���A�XA���A���A�hsA���A�7LA�z�A�ZA�&�A��#A��!A�+A��FA��-A��A���A���A���A���A���A���A��uA��uA��PA��DA��A�z�A�v�A�XA���A���A��jA���A��DA�~�A�z�A�p�A�ffA�`BA�\)A�\)A�\)A�Q�A�K�A�;dA��A�bA��A���A���A�ĜA��FA���A��hA��DA�v�A�r�A�hsA�dZA�XA�G�A�1'A�$�A��A�{A�VA�A���A��A��yA���A��wA���A��DA�hsA�E�A�VA���A���A��A��+A�`BA�A�A�-A��A�VA��`A��;A��jA�t�A��A��#A���A�ƨA��jA��A���A��A�r�A�\)A�G�A�E�A�E�A�9XA�(�A�"�A��A�JA���A��wA��A���A��uA�p�A�jA�\)A�G�A�7LA�&�A���A��TA��
A�ȴA��^A��\A�^5A�E�A�1'A��A�  A��A�{A��-A���A��DA�l�A�Q�A�33A��A��mA�jA�`BA�A�A��`A��^A���A�hsA�$�A�
=A���A��mA��#A��
A���A���A�ȴA���A���A�ĜA��wA�A��^A��DA�Q�A�$�A��RA�|�A�/A�{A�%A��HA���A��PA�^5A�VA�Q�A�O�A�K�A�=qA�(�A�A���A�ffA� �A�%A��A��`A��;A��A���A���A���A��
A���A�ƨA�ĜA�ĜA��FA�~�A��A���A��RA���A���A��+A�p�A�M�A�A�A�A��A��jA��uA��A�jA�S�A�7LA�-A�{A���A��yA���A�S�A�33A��A���A��;A�A��9A���A���A�~�A�O�A�"�A��A�A���A�dZA�bNA��TA��RA��A���A���A���A��PA��A�hsA�=qA��A�A��A��A��mA��`A��`A��`A��;A���A�ƨA���A��-A��A��A���A���A���A��PA�~�A�r�A�hsA�\)A�I�A�5?A�oA���A��A��A��TA���A���A�A��jA��FA��-A���A��+A�ffA�+A��A�ƨA���A�x�A�VA�A�A�7LA��A�ȴA���A��PA�~�A�r�A�/A��`A��\A�jA�=qA�-A�1A��A���A��FA���A��PA�1'A�AAhsA~�HA~�!A~jA~-A}��A}7LA}�A|�/A|A�A{�-A{�PA{K�A{VAz��Az��Az��Az�Az�`Az�yAz�yAz�yAz�yAz�HAz�Az��Az��Az��AzȴAzĜAz��Az�9Az1'Az1Ay�;Ay��Ay�wAy�FAy��Ay\)Ax�jAw��AwhsAw&�Aw&�Aw&�Aw"�Aw�AwVAw
=AwAv��Av��Av�`Av��Av��Av�!Av��Av�uAv�AvjAvffAvbNAv=qAv�Au��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      A�l�A�VA�Q�A�XA�XA�G�A�M�A�G�A�33A�(�A�"�A� �A��A��A��A�oA�JA�%A�A�  A�  A���A��`A��
A��RA���A�~�A�hsA�\)A�1'A��HA��A�z�A�E�A�{A�A��#A�VA���A�ƨA�  A���A�-A��mA�I�A� �A��A���A�O�A��\A�ZA�A���A�K�A�%A���A��wA���A��A�r�A�&�A���A��A�z�A�9XA�A��RA��TA�ƨA�/A��A�I�A�dZA��A��9A��A�A�I�A�z�A��FA���A��`A�l�A��A���A���A�ffA���A��wA�&�A�=qA�ffA�K�A��\A
=A}�A{;dAz�Az�Az�uAyAw�;Aw�Av�HAv�+AvJAu�At�`Ar�+Aq��Ap �Am`BAk��AkVAiC�Ag��Ag`BAg33Ag�Af��Af�yAfr�AeƨAeO�Ac�
AcC�Ab��Aa
=A`bNA`$�A_�wA_�FA_�A_x�A_
=A^�+A^=qA^9XA]VA[K�AZ�AYAX��AV�AV=qAT�AT9XAS�^AR��AP��APM�AOC�AM��AMS�AM
=AL�AK|�AJ�jAJQ�AIx�AIoAH�RAH9XAGC�AF��AE�-ADbNAC|�AC+AB��ABM�AAoA@M�A>��A>A<ȴA<JA;��A;�7A:��A:�\A:9XA9�A8(�A6jA5l�A4~�A3�wA3?}A2�yA2r�A1"�A0JA/�A/7LA.��A.5?A-`BA-�A,��A,jA,bNA,^5A+�
A*VA)�A(�A(�A(I�A&�jA%ƨA$��A#hsA"��A!x�A 1'A�A��A�+AbAbNA�
A�`AE�A�
A��Ap�AS�A33AA�/A��A~�A��A��At�A�\AVAƨAx�A?}A��A�-A�RA��A~�A��A
�HA
  A	�AffA�A��A��A`BA�AA
=Av�A�A�`Ar�AA v�@�|�@�7L@�@���@��@���@��y@�ƨ@�X@�P@�M�@���@���@�&�@�z�@�(�@��@�@�9@�=q@�t�@��@�X@�%@�Ĝ@��;@�K�@�~�@�J@��@݁@�V@�1'@�@��#@١�@���@� �@��y@��@�X@��/@�b@�dZ@�E�@с@�V@Ϯ@�v�@��@�x�@���@�  @�o@�E�@ȴ9@���@¸R@��@�Q�@���@�@�x�@���@�Z@�  @�l�@�n�@���@�&�@�(�@��@�@��T@��`@��P@�
=@�@�hs@��m@�J@��@��@�M�@���@�X@���@��u@�b@�ƨ@��@�;d@���@��@��`@�bN@��F@���@���@���@��j@���@��
@��y@�5?@���@�?}@���@��`@��j@���@���@��R@�J@��-@�/@���@���@�bN@�1'@� �@�  @�ƨ@���@��P@��@�"�@���@��@��h@���@��j@�j@�(�@��w@�;d@��\@�5?@�J@���@���@���@�A�@�b@���@��@��@���@�$�@��#@��h@�%@�Z@�1@��;@��@��P@�;d@��!@�~�@�^5@�M�@�@���@�`B@���@�A�@��
@�C�@��\@�$�@�&�@�b@�|�@�K�@�"�@��@��H@��\@�V@�E�@�J@���@��^@���@��7@��7@�`B@�7L@��@�V@��u@�Q�@�1'@��@�1@�w@;d@}�T@}`B@}?}@}�@|�@|�@|�/@|�/@|��@|�j@|�j@|z�@|Z@|(�@|1@{�m@{��@{t�@z��@z^5@z-@zJ@y��@yhs@yG�@xQ�@w\)@w;d@w�@w
=@v��@v��@v$�@u��@u�-@u�h@u�@u?}@uV@t�j@t9X@s�m@sS�@r�H@r��@r��@r�!@r��@r=q@q��@q�7@qx�@q�@p�u@pb@o�P@o\)@n�@n��@n�+@m�@l��@lj@l1@k�
@k��@kdZ@k"�@ko@j�@j��@j~�@i�7@hĜ@h��@h�@hbN@g\)@f��@f��@f�y@fȴ@f��@fV@e��@d�/@dI�@c�F@c�@cC�@b��@b~�@a�@a��@ahs@aG�@a�@`Ĝ@`A�@_�@_�w@_\)@^��@^V@^$�@]�@]`B@\�/@\��@\�D@[��@[@Z~�@Z�@Y��@Yhs@YG�@Y&�@XQ�@Xb@X  @W�@W��@W
=@V{@U�-@U�@U�@UV@UV@T�D@T(�@S�@SC�@So@R�H@R��@R��@R�@Q�#@Q��@Q��@Q��@QX@Q&�@PĜ@PQ�@PA�@P1'@Pb@O�@O�;@O�;@O�w@O��@O|�@O�@N��@M��@M/@L�@Lz�@K�m@K�
@Kƨ@Kƨ@K��@KdZ@KS�@KC�@K33@K"�@K"�@K"�@K"�@K@J��@J��@JJ@I��@I�@HĜ@Hr�@Hb@G��@G|�@G;d@F��@F�y@F�y@Fȴ@Fv�@E�T@E�-@E��@EO�@D�@D��@D��@D�D@DI�@C��@Ct�@B��@B^5@A��@A�^@AG�@A%@@�9@@r�@@bN@@ �@?�@?�P@?
=@>��@>�+@>v�@>ff@>E�@>5?@>{@=�@=�-@<z�@;S�@:��@:~�@:M�@:J@9��@9X@9�@8��@8�9@8�u@8�@8Q�@8 �@7�w@7|�@7�@6��@6�@6��@6v�@6V@6{@5��@5��@5�@5?}@5�@4�@4�@4j@4I�@49X@4�@3�F@3t�@3dZ@333@3@2�!@2�@1�@1�^@1��@1x�@1X@1G�@0��@0Q�@0  @/|�@/;d@.��@.�@.��@.V@-�T@-��@-��@-�@-`B@-O�@,��@,�@,�@,��@+�
@+S�@+@*�H@*��@*�!@*~�@*=q@*J@)��@)�7@)x�@)X@)X@)%@(Ĝ@(�u@(Q�@(A�@(1'@(b@'�@'��@'�w@'��@'+@&�y@&�R@&��@&v�@&ff@&E�@&$�@&{@%�@%��@%/@%V@$�@$Z@$(�@#��@#��@#�@#S�@"�@"�@"�H@"��@"�!@"�\@"~�@"^5@"J@!�#@!��@!G�@!�@ �`@ r�@ bN@ Q�@  �@  �@ b@�;@�@l�@K�@�@��@��@V@$�@�T@�-@p�@/@V@�/@�j@��@Z@1@��@�m@�
@ƨ@��@t�@dZ@S�@C�@33@�@��@��@^5@�^@��@x�@G�@�@%@��@��@��@r�@ �@b@  @  @�w@|�@K�@+@�@��@�R@�+@$�@{@{@�@�-@`B@/@�@��@�/@Z@��@��@��@�
@��@o@�H@��@��@�!@�\@~�@M�@J@J@��@�#@x�@��@�9@��@�u@r�@Q�@1'@ �@�@��@�w@��@l�@��@V@V@E�@{@��@�@/@��@�j@�@j@9X@(�@(�@�@1@��@�
@��@t�@dZ@S�@C�@
�@
�!@
��@
��@
�\@
n�@
^5@
M�@
�@	��@	�#@	X@	G�@	&�@	�@�`@�@A�@A�@�;@�@��@��@�P@|�@|�@l�@l�@K�@�y@��@�+@v�@ff@ff@V@5?@{@�T@�h@p�@?}@?}@?}@/@��@��@��@�j@�D@z�@Z@�@�@��@ƨ@�F@��@��@t�@dZ@S�@33@o@@�H@��@�\@~�@^5G�O�A�hsA�jA�l�A�`BA�VA�XA�S�A�S�A�VA�S�A�Q�A�S�A�S�A�ZA�XA�\)A�XA�VA�XA�K�A�A�A�O�A�G�A�I�A�O�A�Q�A�O�A�O�A�E�A�=qA�1'A�1'A�-A�-A�+A�&�A�$�A�$�A�"�A� �A� �A�"�A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A��A��A��A��A��A��A��A��A��A�VA�JA�JA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�1A�%A�%A�1A�%A�1A�%A�A�A�A�A�A�A�  A�  A���A���A���A���A���A���A���A�  A�  A�  A�A�A�A�  A�  A���A�  A�  A���A��yA��mA��mA��`A��TA��HA��TA��HA��;A��/A��/A��#A��#A��#A�ȴA���A��wA��jA��RA��jA��A��!A��A��A���A���A���A��A��A��A��A��A��A�|�A�|�A�z�A�r�A�n�A�l�A�l�A�jA�dZA�bNA�`BA�`BA�\)A�\)A�\)A�ZA�\)A�S�A�O�A�G�A�;dA�33A�oA�bA���A���A��A��`A��#A���A�ƨA��wA��^A��FA��-A��A��A���A��hA��DA�~�A�z�A�z�A�r�A�ffA�bNA�^5A�Q�A�C�A�7LA�5?A�33A��A��A�{A��A��A�bA�JA�1A�JA�
=A�1A�A���A���A���A���A���A��;A���A���A��^A��A�|�A�p�A�^5A�G�A�7LA�-A�"�A��A�JA�A��A��TA���A��PA�&�A��A��
A��A�r�A�ZA�M�A�9XA�oA���A��`A�ȴA���A��wA��A���A���A��hA��A�p�A�\)A�O�A�9XA�$�A�oA�A���A���A���A�ZA��jA�ZA��A���A���A�XA���A���A�hsA���A�7LA�z�A�ZA�&�A��#A��!A�+A��FA��-A��A���A���A���A���A���A���A��uA��uA��PA��DA��A�z�A�v�A�XA���A���A��jA���A��DA�~�A�z�A�p�A�ffA�`BA�\)A�\)A�\)A�Q�A�K�A�;dA��A�bA��A���A���A�ĜA��FA���A��hA��DA�v�A�r�A�hsA�dZA�XA�G�A�1'A�$�A��A�{A�VA�A���A��A��yA���A��wA���A��DA�hsA�E�A�VA���A���A��A��+A�`BA�A�A�-A��A�VA��`A��;A��jA�t�A��A��#A���A�ƨA��jA��A���A��A�r�A�\)A�G�A�E�A�E�A�9XA�(�A�"�A��A�JA���A��wA��A���A��uA�p�A�jA�\)A�G�A�7LA�&�A���A��TA��
A�ȴA��^A��\A�^5A�E�A�1'A��A�  A��A�{A��-A���A��DA�l�A�Q�A�33A��A��mA�jA�`BA�A�A��`A��^A���A�hsA�$�A�
=A���A��mA��#A��
A���A���A�ȴA���A���A�ĜA��wA�A��^A��DA�Q�A�$�A��RA�|�A�/A�{A�%A��HA���A��PA�^5A�VA�Q�A�O�A�K�A�=qA�(�A�A���A�ffA� �A�%A��A��`A��;A��A���A���A���A��
A���A�ƨA�ĜA�ĜA��FA�~�A��A���A��RA���A���A��+A�p�A�M�A�A�A�A��A��jA��uA��A�jA�S�A�7LA�-A�{A���A��yA���A�S�A�33A��A���A��;A�A��9A���A���A�~�A�O�A�"�A��A�A���A�dZA�bNA��TA��RA��A���A���A���A��PA��A�hsA�=qA��A�A��A��A��mA��`A��`A��`A��;A���A�ƨA���A��-A��A��A���A���A���A��PA�~�A�r�A�hsA�\)A�I�A�5?A�oA���A��A��A��TA���A���A�A��jA��FA��-A���A��+A�ffA�+A��A�ƨA���A�x�A�VA�A�A�7LA��A�ȴA���A��PA�~�A�r�A�/A��`A��\A�jA�=qA�-A�1A��A���A��FA���A��PA�1'A�AAhsA~�HA~�!A~jA~-A}��A}7LA}�A|�/A|A�A{�-A{�PA{K�A{VAz��Az��Az��Az�Az�`Az�yAz�yAz�yAz�yAz�HAz�Az��Az��Az��AzȴAzĜAz��Az�9Az1'Az1Ay�;Ay��Ay�wAy�FAy��Ay\)Ax�jAw��AwhsAw&�Aw&�Aw&�Aw"�Aw�AwVAw
=AwAv��Av��Av�`Av��Av��Av�!Av��Av�uAv�AvjAvffAvbNAv=qAv�Au��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B1B�B�B+B_B�B1BeB1B�B�B�B1B�B1B�BeB1B�B_B_B�BB�B�B1B%B�B�BABoB �B iB�PB�xB��B��B�GB��B�,B�QB�TB�RB�tB��B��B��B��B�oB�B~]Br�Bk�Be,B]�BMjB:�B'BeB B�B�JB��B��B��B�:B��B�lB�YBncB^BWsBGEBEB9XB2�B'�B 'BbB�B�%B�B��B��B�B�gB��B�RB�9B��B�B��B�:B�SB|�BkQBgBd�Bd&B\�BW�BL0BK^BG�BD�B?B:*B/�B#B �B�B{B
��B
�xB
�B
��B
�fB
��B
�B
�B
�B
�jB
��B
�&B
��B
ȴB
� B
�0B
�^B
��B
��B
��B
��B
��B
�[B
��B
��B
�B
��B
��B
��B
�7B
�lB
��B
z�B
w2B
s�B
o�B
d�B
`BB
]/B
R�B
QNB
N�B
L�B
H�B
C�B
A�B
>�B
;dB
8�B
7�B
33B
0�B
.B
&�B
$@B
!bB
�B
�B
�B
uB
�B
SB
 �B	�"B	��B	��B	�`B	��B	�B	�B	�B	�B	�)B	�yB	�aB	��B	�B	ɺB	��B	��B	�<B	��B	��B	�B	�-B	��B	�}B	�=B	�kB	�eB	�eB	�@B	��B	�CB	��B	�B	�B	��B	�1B	�4B	|�B	{B	w2B	r�B	p�B	l"B	e�B	Z�B	XyB	VmB	T,B	R�B	Q�B	P�B	PB	OvB	OB	M�B	MB	J�B	J#B	FB	B�B	=�B	<�B	<6B	9�B	8RB	6FB	5?B	/�B	/�B	)�B	(�B	&LB	#B	"�B	 �B	!B	�B	�B	B	B	�B	1B	�B	�B	1B	$B	�B	~B	qB	�B	$B	�B	uB	4B	�B	FB	�B	(B	�B	�B	
�B	B	
	B		lB		B	+B	�B	�B	DB	�B	_B	YB	�B	�B	�B	YB	�B	SB	�B	B	�B	�B	�B	�B	�B	�B	�B	SB	�B	�B	�B	�B	MB	B	B	�B	�B	�B	uB	�B	�B	�B	�B	SB		lB	PB	�B	B	.B	.B	�B	�B	�B	�B	:B	uB	@B	�B	SB	�B	YB	�B	�B	�B	B	!B	�B	#B	'RB	,B	+kB	.IB	/OB	/OB	0UB	1'B	2aB	3�B	3�B	2�B	7LB	9$B	9XB	:�B	<�B	?HB	EB	F�B	F�B	F�B	K^B	OB	Q�B	TaB	T�B	UgB	T�B	U�B	Y�B	Y�B	^5B	aB	b�B	e�B	f�B	gB	iyB	i�B	jB	j�B	l"B	l�B	l�B	l�B	n�B	qAB	tTB	x8B	zDB	{B	|�B	}�B	�B	��B	�YB	��B	�fB	��B	��B	�bB	�oB	�@B	�B	�SB	��B	�~B	�'B	��B	��B	�RB	�=B	��B	��B	��B	��B	��B	�zB	��B	�RB	��B	�^B	��B	�BB	�[B	�B	�RB	͟B	҉B	��B	�dB	��B	�KB	�B	��B	��B	�cB	�AB	��B	�MB	��B	�2B	�B	��B	�rB	�rB	��B	�PB	�VB	�VB
B
�B
�B
�B
�B
+B
	7B
�B
�B
:B
B
�B
�B
B
FB
�B
B
MB
SB
�B
�B
+B
�B
B
�B
B
�B
 \B
 �B
"4B
$B
$tB
)_B
-CB
-�B
.}B
.�B
.�B
0�B
3�B
5tB
6FB
6zB
7B
8B
8�B
:*B
<jB
>BB
A B
CaB
C�B
C�B
C�B
C�B
FtB
IB
IRB
I�B
K)B
MB
O�B
QB
P�B
S[B
T�B
T�B
WsB
Y�B
ZQB
[#B
\)B
]�B
^�B
`BB
aB
aB
a|B
b�B
e�B
gmB
gmB
g8B
g8B
m)B
n�B
o B
o B
o�B
pB
poB
r|B
u�B
xlB
z�B
{�B
|�B
�B
��B
�B
�SB
�YB
��B
��B
�lB
�JB
�"B
��B
��B
��B
�B
��B
�{B
��B
�+B
�_B
�_B
��B
�~B
��B
��B
�B
�@B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�UB
��B
�-B
�-B
��B
�B
�B
�LB
�B
��B
�$B
�$B
��B
�dB
�6B
�6B
�jB
�B
�wB
��B
�B
�UB
��B
��B
�'B
B
B
B
��B
�-B
�aB
��B
�B
�B
�#B
�#B
��B
�B
�6B
�jB
�6B
��B
�pB
�pB
�pB
��B
�B
��B
��B
��B
�BB
�B
�HB
��B
��B
ԕB
��B
��B
�?B
��B
�yB
خB
�B
�KB
�KB
ٴB
�QB
یB
��B
یB
�]B
�dB
�B
��B
�B
�jB
�;B
�vB
�B
��B
�B
�&B
�,B
��B
�fB
�B
�mB
�
B
�B
�DB
�B
�QB
�QB
�QB
�QB
�B
�B
�B
�B
�B
�B
�oB
�AB
�B
�B
�|B
�B
�B
�B
�%B
�%B
�%B
�%B
��B
��B
��B
�2B
�B
�lB
��B
�	B
�>B
�rB
�DB
�xB
��B
��B
�JB
�JB
��B
�B
��B
��B
�B
�B
��B
�"B
��B
��B
��B
�]B
��B
��B 4B iB iB �BB�BuBGBBMB�B�B�B�B�B�B�B_B_B�B1B�B�B�B	�B
�B�B�BB�B~B�B�B�B�B�B�B�B�B�B(B�B�B�B�B.BbBbB B�BB�B�B�BBB@B�B�BFB�BBB�BSB�BYB$B�B_B�B�B+B_B_B+B�BeB�B7B�B=B�B�BB�B�B�B�B�B!BVB�B�B�B �B �B �B!�B!�B"4B"hB"�B#:B#B#:B#�B$@B$�B$�B$�B%B&B&�B&�B&�B&�B'RB(XB(XB(XB(�B)�B)�B)�B*0B*�B*�B+B+kB+6B+kB,=B,B+�B*�B*�B*�B+kB+�B+�B,=B,qB,�B-wB-CB-wB-CB-�B.}B.}B.�B.�B.�B/�B/�B/�B/�B/�B0�B1'B1'B1'B0�B1'B1'B1'B1[B1�B1[B1[B1[B1�B2aB2�B2�B2�B33B3hB3hB3�B49B4nB49B4nB4�B6FB6zB6zB6�B7B7�B7�B8B8B7�B8B8�B8�B8�B8�B8�B9$B9$B9�B9�B:^B:^B:�B:�B;0B;�B;dB;�B;�B;�B<B<jB<�B=B=<B>B=�B=�B=�B>B>BB>wB>�B?�B?}B?�B?}B?�B?}B?�B?�B?�B?�B@�B@�B@�B@�B@�B@�BA BAUBAUBA�BA�BA�BB[BB'BA�BA�BB'BB[BB'BB'BA�BA�BB'BB'BA�BA�BA B?B?B?B?HB?HB?HB?}B?�B?�B?�B@�B@BAUBA�BB[BB�B�BIB	B�B�B�B�B+B�B�B_B�B�B�B�BB7B�B�B�B�B�B�B�B�B�B	B1BB�B�B7B1B�B1B�B�BeB�B�B�B�B�B1B�BeB�B�B+B+B_B�B�B_B�B�B�B�B7B�BB�B$B�B�B7B1BkBBxB�BB�BeB1BeBeB�B�B�B�B1B�B+B�BeB�B�B�B�B�B�B�B_B�B�B�B�B1B1BeBeB�B�B+B�B�B�B+B�B�B1B�B�B7BYB�BSBSBSB�BMB�B�BBBB�B�BFB:B�B\B�B�B�B�B�B"B�B�BPB"B�B1BfB1BfB1BfB1B_B�B+B�B�BYBB�B�BSBSBMBMB�B�B{BGBB�B�B �B�BuBuBAB�BB�BoB;B�B�BBAB  B �B�B�BB��B��B��B��B��BoBAB �B 4B��B�xB��B��B�B�"B�B�B��B�B��B��B�JB�JB��B��B�fB��B��B��B�TB��B��B�+B�B�MB�MB�B�B�B�B�AB��B�TB�vB�B�B��B�oB�B�/B�
B�fB�8B�B�fB��B�B��BݘB�]B�/BٴBخB�#B��B��B��B�[B�9B��B�B�6B͟B�&B�B��B�BB��B��B�hB��B��B��B��B��B�B�B��B��B�:B�JB��B�=B�MB��B��B��B��B�MB�B��B��B�B�B�uB��B�GB�B��B�4B}�B��B��B��B��B� B�oB��B�4B�B�4B�4B�uB� B��B�B|�B�oBv`By	Bv`BuZBs�Bs�BpBqBl�Bm�BlWBm�Bl�Bk�Bh
Bg8Bg8Bd�Be`Bb�Be`BcTB`vB_pB]/B_�BX�BW�BRTBOBBN�BLdBK�BF�B@�B>BB=<B;0B8�B4�B;�B49B1�B$B!-B�BB�B�B	B�B�BFBoBhBFB�B(B�B�B�B	B�BSB�BMB 4B�B��B��B��B�B�%B�B�oB�oB�B��B�8B�TB� B�B��B%B��BƨB�B�'B�B��B�RB��B��B��B�XB�tB�IB��B�YB��B��B��B��B�VB��B�JB�rB��B��B�B��B��B��B��B�rB��B��B��BcBx�Bp�Bm�BqABgBtBd�B_pB^5B\�B\)B^�B_�B^5BbB_pBT�BN�BM�BGBH�BH�BJ#BF�BEBDgBE�BC�BDgBA�BC�BJ�BI�B?HB7�B7LB6�B2aB2�B5�B1'B0�B/�B1�B-wB'�B&�B($B&�B+B%FB"�B!�B-CBBCB�B�B�B�BhBB
�B�B�B�B�B
=B\B�B8�B�B�KB��B��B��B��B��B�ZB�8B�
B�TB�B�/B�WB�/B�/B�)B�#BݘB��B��BچBچB�KB�yB�yB��B�?BרB��B�mBԕB�&B��B�aB֡B�BB��B��B��B�<BɺB�BȀB�KB��BɺB�B�?B�B�3B�HB�qB�$B��B��B��B�hB�B�B��B�B��B��B�$B�B�-B�B��B�	B��B�B�B�(B��B��B��B��B�\B��B��B��B~�B��BxBt�Bw�B�BncBp;Bm�Bn/Be�Be�Bg�BffBf�Be�Bf�BffBf2Bf2Bd&Bc�Bc Bc�Bc BbBbBbNBsMBcTB`B\�B\�BZ�BZBbBdZB^BZ�BOBBK^BK�BK�BL�BL�BK)BM6BK�BJ�BL�BK�BJ#BIBH�BG�BGzBH�BFtBF?BG�BE�BEG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021031100383720210311003837IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021032100054420210321000544QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021032100054420210321000544QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164620210325101646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005320210326170053IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005320210326170053IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005320210326170053IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                